using System;
using System.Collections.Generic;
using System.Linq;
using JymlAST;
using JymlEnvironment;
using JymlParser;
using JymlTypeSystem;

namespace Interpreter {
    static class SuckerScriptInterpreter {
        public static Cons Eval(Cons exp, JymlEnvironment.JymlEnvironment env) {
            if (exp != null) {
                if (Parser.IsSelfEvaluating(exp)) {
                    return exp;
                }
                else if (Parser.IsVariable(exp)) {
                    return new Cons(env.FrameNode[exp.car as string].Value);
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
                        Eval(exp.car as Cons, env),
                        ListOfValues(exp.cdr as Cons, env)
                    );
                }
            }
        }

        private static Cons MakeLambda(string[] parameters, Cons body, JymlEnvironment.JymlEnvironment env) {
            return new Cons(new Procedures(parameters, body, env));
        }

        /*
         * (define (eval-definition exp env)
                (define-variable!   (definition-variable exp)
                                    (eval (definition-value exp) env)
                                    env))
         */
        private static Cons EvalDefinition(Cons exp, JymlEnvironment.JymlEnvironment env) {
            /*
             * (define (definition-variable exp) 
                    (if [symbol? [mcar [mcdr exp]]]
                        [mcar [mcdr exp]]
                        [mcar [mcar [mcdr exp]]]))
             */
            string defineVariable(Cons c) =>
                Parser.IsVariable(c.cdr as Cons) ? (c.cdr as Cons).car as string  // 普通变量或 lambda 变量
                                                 : ((c.cdr as Cons).car as Cons).car as string;   // 过程定义

            /*
             * (define (definition-value exp) 
                    (if [symbol? [mcar [mcdr exp]]]
                        [mcar [mcdr [mcdr exp]]]
                        (make-lambda    [mcdr [mcar [mcdr exp]]]    ; formal parameters
                                        [mcdr [mcdr exp]])))        ; body
             */
            JymlType defineValue(Cons c) =>
               Parser.IsVariable((c.cdr as Cons).car as Cons) ? ((c.cdr as Cons).cdr as Cons).car as Cons  // 普通变量或 lambda 变量
                                                              : MakeLambda(Parser.GetLambdaParameters(exp), Parser.GetLambdaBody(exp), env);   // 过程定义
            env.DefineVariable(defineVariable(exp), defineValue(exp));
            return null;
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
        private static Cons Apply(Cons proc, Cons arguments) {
            if (proc.car is string procedureName) {
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
                    Procedures p = proc.car as Procedures;
                    string[] variables = p.Parameters.ToArray<string>();
                    JymlType[] values = p.Arguments.ToArray<JymlType>();
                    return EvalSequence(
                        cons: proc,
                        env: p.Environment.ExtendEnvironment(p.Parameters, p.Arguments)
                    );
                }
            }
            else {
                throw new Exception();
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
