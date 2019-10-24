using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using JymlAST;
using JymlEnvironment;
using JymlParser;
using JymlTypeSystem;

namespace Interpreter {
    static class SuckerScriptInterpreter {
        public static Cons Eval(Cons exp, JymlEnvironment.JymlEnvironment env) {
            if (Parser.IsSelfEvaluating(exp)) {
                return exp;
            }
            else if (Parser.IsVariable(exp)) {
                return new Cons(env.FrameNode[exp.car as string].Variable);
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
                return EvalSequence(exp.cdr as Cons, env);
            }
            else {
                return Apply(
                    Eval(exp.car as Cons, env).car as string,
                    ListOfValues(exp.cdr as Cons, env)
                );
            }
        }

        /*
         * ;; eval-sequence 用在 apply 里，用于求值过程体里的表达式序列。它也用在 eval 里，用于
           ;; 求值 begin 表达式里的表达式序列。这个过程以一个表达式序列和一个环境为参数，按照序列里
           ;; 的表达式出现的顺序对它们求值。它返回最后一个表达式的值。
           (define (eval-sequence exps env)
               (cond   [(last-exp? exps) (eval (first-exp exps) env)]
                       [else   (eval (first-exp exps) env)
                               (eval-sequence (rest-exps exps) env)]))
         */
        private static Cons EvalSequence(Cons cons, JymlEnvironment.JymlEnvironment env) {
            if (cons.cdr == null) {
                return Eval(cons.car as Cons, env);
            }
            else {
                return EvalSequence(cons.cdr as Cons, env);
            }
        }

        /*
         * (define (apply procedure arguments)
                (cond   [(primitive-procedure? procedure)
                        (apply-primitive-procedure procedure arguments)]
                        [(compound-procedure? procedure)
                        (eval-sequence (procedure-body procedure) (extend-enviroment
                                                                    (procedure-parameters procedure)
                                                                    arguments
                                                                    (procedure-enviroment procedure)))]
                        [else (error "Unknown procedure type -- Apply" procedure)]))
         */
        private static Cons Apply(string procedureName, Cons arguments) {
            if (PrimitiveProcedure.PrimitiveProcedures.Keys.Contains(procedureName)) {
                // (apply-primitive-procedure procedure arguments)
                return new Cons(PrimitiveProcedure.PrimitiveProcedures[procedureName].Invoke(arguments.ToArray()));
            }
            else {
                /*
                 * (eval-sequence (procedure-body procedure) (extend-enviroment
                                                        (procedure-parameters procedure)
                                                        arguments
                                                        (procedure-enviroment procedure)))
                 */
                return EvalSequence(
                    cons:
                    env:
                 );
            }
        }

        /*
         * eval 在处理过程应用时用 list-of-values 去生成实际参数表，以便完成这一过程应用。
           list-of-values 以组合式的运算对象作为参数，求值各个运算对象，返回这些值的表

           (define (list-of-values exps env)
               (if [no-operands? exps]
                   null
                   (mcons  (eval (car exps) env)
                           (list-of-values (cdr exps) env))))
         */
        private static Cons ListOfValues(Cons exp, JymlEnvironment.JymlEnvironment env) =>
            new Cons(Eval(exp.car as Cons, env), ListOfValues(exp.cdr as Cons, env));
    }
}
