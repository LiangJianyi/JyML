using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using JymlAST;
using JymlParser;
using JymlTypeSystem;

namespace Interpreter {
    static class SuckerScriptInterpreter {
        public static Cons Eval(Cons exp, JymlEnviroment env) {
            if (Parser.IsSelfEvaluating(exp)) {
                return exp;
            }
            else if (Parser.IsVariable(exp)) {
                return new Cons(env.Frame[exp]);
            }
            else if (Parser.IsAssignment(exp)) {
                return EvalAssignment(exp, env);
            }
            else if (Parser.IsDefinition(exp)) {
                return EvalDefinition(exp, env);
            }
            else if (Parser.IsIf(exp)) {
                return EvalIf(exp, env);
            }
            else if (Parser.IsLambda(exp)) {
                return MakeLambda(Parser.GetLambdaParameters(exp), Parser.GetLambdaBody(exp), env);
            }
            else if (Parser.IsBegin(exp)) {
                return EvalSequence(BeginActions(exp));
            }
        }
    }
}
