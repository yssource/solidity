/*
	This file is part of solidity.

	solidity is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	solidity is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with solidity.  If not, see <http://www.gnu.org/licenses/>.
*/
/**
 * @author Alex Beregszaszi
 * @date 2017
 * Component that translates Solidity code into Yul.
 */

#include <libsolidity/codegen/ir/IRGenerator.h>

#include <libsolidity/ast/AST.h>
#include <libsolidity/codegen/ABIFunctions.h>

#include <libyul/AssemblyStack.h>

#include <libdevcore/CommonData.h>
#include <libdevcore/Whiskers.h>
#include <libdevcore/StringUtils.h>

#include <liblangutil/SourceReferenceFormatter.h>

#include <boost/algorithm/string/predicate.hpp>

using namespace std;
using namespace dev;
using namespace dev::solidity;

string IRGenerator::run(ContractDefinition const& _contract)
{
	string ir = generateIR(_contract);

	yul::AssemblyStack asmStack(m_evmVersion, yul::AssemblyStack::Language::StrictAssembly);
	if (!asmStack.parseAndAnalyze("", ir))
	{
		string errorMessage;
		for (auto const& error: asmStack.errors())
			errorMessage += langutil::SourceReferenceFormatter::formatExceptionInformation(
				*error,
				(error->type() == langutil::Error::Type::Warning) ? "Warning" : "Error"
			);
		solAssert(false, "Invalid IR generated:\n" + errorMessage);
	}
	// TODO we need some way to retain comments
	return asmStack.print();
}

string IRGenerator::generateIR(ContractDefinition const& _contract)
{
	Whiskers t(R"(
		object "<CreationObject>" {
			code {
				<constructor>
				<deploy>
				<functions>
			}
			object "<RuntimeObject>" {
				code {
					<dispatch>
					<runtimeFunctions>
				}
			}
		}
	)");
	m_context = IRGenerationContext(m_evmVersion, m_optimiserSettings);

	t("CreationObject", creationObjectName(_contract));
	t("constructor", _contract.constructor() ? constructorCode(*_contract.constructor()) : "");
	t("deploy", deployCode(_contract));
	t("functions", m_context.functionCollector()->requestedFunctions());

	m_context = IRGenerationContext(m_evmVersion, m_optimiserSettings);
	t("RuntimeObject", runtimeObjectName(_contract));
	t("dispatch", dispatchRoutine(_contract));
	t("runtimeFunctions", m_context.functionCollector()->requestedFunctions());
	return t.render();
}

string IRGenerator::generateIRFunction(FunctionDefinition const& _function)
{
	string functionName = "fun_" + to_string(_function.id()) + "_" + _function.name();
	return m_context.functionCollector()->createFunction(functionName, [&]() {
		Whiskers t("function <functionName>(<params>) <returns> {}");
		t("functionName", functionName);
		string params;
		for (auto const& varDecl: _function.parameters())
			params += (params.empty() ? "" : ", ") + m_context.addLocalVariable(*varDecl);
		t("params", params);
		string retParams;
		for (auto const& varDecl: _function.returnParameters())
			retParams += (retParams.empty() ? "" : ", ") + m_context.addLocalVariable(*varDecl);
		t("returns", retParams.empty() ? "" : " -> " + retParams);
		return t.render();
	});
}

string IRGenerator::constructorCode(FunctionDefinition const& _constructor)
{
	string out;
	if (!_constructor.isPayable())
		out = callValueCheck();

	return out;
}

string IRGenerator::deployCode(ContractDefinition const& _contract)
{
	Whiskers t(R"X(
		codecopy(0, dataoffset("<object>"), datasize("<object>"))
		return(0, datasize("<object>"))
	)X");
	t("object", runtimeObjectName(_contract));
	return t.render();
}

string IRGenerator::callValueCheck()
{
	return "if callvalue() { revert(0, 0) }";
}

string IRGenerator::creationObjectName(ContractDefinition const& _contract)
{
	return _contract.name() + "_" + to_string(_contract.id());
}

string IRGenerator::runtimeObjectName(ContractDefinition const& _contract)
{
	return _contract.name() + "_" + to_string(_contract.id()) + "_deployed";
}

string IRGenerator::dispatchRoutine(ContractDefinition const& _contract)
{
	Whiskers t(R"X(
		if iszero(lt(calldatasize(), 4))
		{
			let selector := <shr224>(calldataload(0))
			switch selector
			<#cases>
			case <functionSelector>
			{
				// <functionName>
				<callValueCheck>
				<assignToParams> <abiDecode>(4, calldatasize())
				<assignToRetParams> <function>(<params>)
				let memPos := <allocate>(0)
				let memEnd := <abiEncode>(memPos <comma> <retParams>)
				return(memPos, sub(memEnd, memPos))
			}
			</cases>
			default {}
		}
		<fallback>
	)X");
	t("shr224", m_utils.shiftRightFunction(224));
	vector<map<string, string>> functions;
	for (auto const& function: _contract.interfaceFunctions())
	{
		functions.push_back({});
		map<string, string>& templ = functions.back();
		templ["functionSelector"] = "0x" + function.first.hex();
		FunctionTypePointer const& type = function.second;
		templ["functionName"] = type->externalSignature();
		templ["callValueCheck"] = type->isPayable() ? "" : callValueCheck();
		// TODO could generate nice names.
		string params = variableList(make_shared<TupleType>(type->parameterTypes())->sizeOnStack(), "param_");
		string retParams = variableList(make_shared<TupleType>(type->returnParameterTypes())->sizeOnStack(), "ret_");
		templ["assignToParams"] = params.empty() ? "" : "let " + params + " := ";
		templ["assignToRetParams"] = retParams.empty() ? "" : "let " + retParams + " := ";

		ABIFunctions abiFunctions(m_evmVersion, m_context.functionCollector());
		templ["abiDecode"] = abiFunctions.tupleDecoder(type->parameterTypes());
		templ["params"] = params;
		templ["retParams"] = retParams;

		templ["function"] = generateIRFunction(dynamic_cast<FunctionDefinition const&>(type->declaration()));

		templ["allocate"] = m_utils.allocationFunction();
		// TODO The values should be in reverse!
		templ["abiEncode"] = abiFunctions.tupleEncoder(type->returnParameterTypes(), type->returnParameterTypes(), false);
		templ["comma"] = retParams.empty() ? "" : ", ";
	}
	t("cases", functions);
	if (FunctionDefinition const* fallback = _contract.fallbackFunction())
	{
		string fallbackCode;
		if (!fallback->isPayable())
			fallbackCode += callValueCheck();
		fallbackCode += generateIRFunction(*fallback) + "() stop()";

		t("fallback", fallbackCode);
	}
	else
		t("fallback", "revert(0, 0)");
	return t.render();
}

void IRGenerator::resetContext()
{
	solAssert(
		m_context.functionCollector()->requestedFunctions().empty(),
		"Reset context while it still had functions."
	);
	m_context = IRGenerationContext(m_evmVersion, m_optimiserSettings);
	m_utils = YulUtilFunctions(m_evmVersion, m_context.functionCollector());
}

string IRGenerator::variableList(size_t _amount, string const& _prefix)
{
	string list;
	for (unsigned i = 0; i < _amount; i++)
		list += (list.empty() ? "" : ", ") + _prefix + to_string(i + 1);
	return list;
}

