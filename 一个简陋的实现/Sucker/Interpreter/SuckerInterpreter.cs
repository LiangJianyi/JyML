using System;
using JymlParser;
using JymlAST;

namespace Interpreter {
    public static class SuckerInterpreter {
        public static object Eval((Language lang, Cons ast) dispatch) {
            switch (dispatch.lang) {
                case Language.SuckerML:
                    return SuckerMLInterpreter.Eval(dispatch.ast);
                case Language.SuckerScript:
                    return SuckerScriptInterpreter.Eval(dispatch.ast, Jyml.Environment.JymlEnvironment.SetUpEnvironment());
                default:
                    throw new FormatException($"语言指示符不存在：#lang {dispatch.lang}");
            }
        }
    }
}
