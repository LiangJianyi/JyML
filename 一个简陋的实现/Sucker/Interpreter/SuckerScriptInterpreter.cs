﻿using System;
using System.Linq;
using JymlParser;
using JymlAST;
using Jyml.Environment;
using JymlTypeSystem;

namespace Interpreter {
    public static class SuckerScriptInterpreter {
        public static JymlAST.Cons Eval(JymlAST.Cons exp, JymlEnvironment env) {
            if (exp != null) {
                if (Parser.IsSelfEvaluating(exp)) {
                    return new JymlAST.Cons(exp.car, Eval(exp.cdr as JymlAST.Cons, env));
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
                    return EvalSequence(exp.cdr as JymlAST.Cons, env);
                }
                else if (exp.car is JymlAST.Cons subexp) {
                    return new JymlAST.Cons(Eval(subexp, env), Eval(exp.cdr as JymlAST.Cons, env));
                }
                else {
                    if (exp.car is string proc) {
                        JymlEnvironment.Restraint restraint = env[proc];
                        if (restraint != null) {
                            if (restraint.Value is Procedures || restraint.Value is PrimitiveProcedure) {
                                return Apply(proc, ListOfValues(exp.cdr as JymlAST.Cons, env), env);
                            }
                            else {
                                return new JymlAST.Cons(restraint.Value, Eval(exp.cdr as JymlAST.Cons, env));
                            }
                        }
                        else {
                            if (Parser.IsVariable(exp)) {
                                throw new Exception($"变量 {exp.car} 未定义。");
                            }
                            else {
                                throw new Exception($"无效的语法：{exp}");
                            }
                        }
                    }
                    else if (exp.car is JymlAST.Cons cons) {
                        return Apply(cons, ListOfValues(exp.cdr as JymlAST.Cons, env), env);
                    }
                    else {
                        throw new Exception($"无效的过程调用：{exp}");
                    }
                }
            }
            else {
                return null;
            }
        }

        /*
         * ;; eval-assignment 调用 eval 找出需要赋的值，将变量和得到的值传给过程 set-variable-value!，
           ;; 将有关的值安置到指定环境里
           (define (eval-assignment exp env)
               (set-variable-value!    (assignment-variable exp)
                                       (eval (assignment-value exp) env)
                                       env)
               'ok)
         */
        private static JymlAST.Cons EvalAssignment(JymlAST.Cons exp, JymlEnvironment env) {
            env.SetVariableValue(
                var: (exp.cdr as JymlAST.Cons).car as string,
                val: JymlType.CreateType(((exp.cdr as JymlAST.Cons).cdr as JymlAST.Cons).car as string)
            );
            return null;
        }

        /*
         * ;; eval-if 在给定环境中求值 if 表达式的谓词部分，如果得到的结果为真，eval-if 就去求值这个 if 的
           ;; consequent 部分，否则求值其 alternative 部分
           (define (eval-if exp env)
               (if [true? (eval (if-predicate exp) env)]
                   (eval (if-consequent exp) env)
                   (eval (if-alternative exp) env)))
         */
        private static JymlAST.Cons EvalIf(JymlAST.Cons exp, JymlEnvironment env) {
            JymlAST.Cons predicate;
            if ((exp.cdr as JymlAST.Cons).car is JymlAST.Cons predicateCons) {
                predicate = predicateCons;
            }
            else if ((exp.cdr as JymlAST.Cons).car is string predicateString) {
                predicate = new JymlAST.Cons(predicateString);
            }
            else {
                throw new Exception($"if 表达式 predicate 部分解析错误，表达式：{exp}");
            }
            JymlAST.Cons consequent;
            if (((exp.cdr as JymlAST.Cons).cdr as JymlAST.Cons).car is JymlAST.Cons consequentCons) {
                consequent = consequentCons;
            }
            else if (((exp.cdr as JymlAST.Cons).cdr as JymlAST.Cons).car is string consequentString) {
                consequent = new JymlAST.Cons(consequentString);
            }
            else {
                throw new Exception($"if 表达式 consequent 部分解析错误，表达式：{exp}");
            }
            JymlAST.Cons alternative;
            if (((exp.cdr as JymlAST.Cons).cdr as JymlAST.Cons).cdr == null) {
                alternative = null;
            }
            else if ((((exp.cdr as JymlAST.Cons).cdr as JymlAST.Cons).cdr as JymlAST.Cons).car is JymlAST.Cons alternativeCons) {
                alternative = alternativeCons;
            }
            else if ((((exp.cdr as JymlAST.Cons).cdr as JymlAST.Cons).cdr as JymlAST.Cons).car is string alternativeString) {
                alternative = (JymlAST.Cons)alternativeString;
            }
            else {
                throw new Exception($"if 表达式 alternative 部分解析错误，表达式：{exp}");
            }

            if ((Eval(predicate, env).car as JymlTypeSystem.Boolean)) {
                return Eval(consequent, env);
            }
            else {
                return Eval(alternative, env);
            }
        }

        private static JymlAST.Cons MakeLambda(string[] parameters, JymlAST.Cons body, JymlEnvironment env) {
            return new JymlAST.Cons(new Procedures((JymlAST.Cons)parameters, body, env));
        }

        /*
         * (define (eval-definition exp env)
                (define-variable!   (definition-variable exp)
                                    (eval (definition-value exp) env)
                                    env))
         */
        private static JymlAST.Cons EvalDefinition(JymlAST.Cons exp, JymlEnvironment env) {
            /*
             * (define (definition-variable exp) 
                    (if [symbol? [mcar [mcdr exp]]]
                        [mcar [mcdr exp]]
                        [mcar [mcar [mcdr exp]]]))
             */
            string defineVariable(JymlAST.Cons c) =>
                Parser.IsVariable(c.cdr as JymlAST.Cons) ? (c.cdr as JymlAST.Cons).car as string  // 普通变量或 lambda 变量
                                                         : ((c.cdr as JymlAST.Cons).car as JymlAST.Cons).car as string;   // 过程定义

            /*
             * (define (definition-value exp) 
                    (if [symbol? [mcar [mcdr exp]]]
                        [mcar [mcdr [mcdr exp]]]
                        (make-lambda    [mcdr [mcar [mcdr exp]]]    ; formal parameters
                                        [mcdr [mcdr exp]])))        ; body
             */
            JymlType defineValue(JymlAST.Cons c) {
                if (Parser.IsVariable(c.cdr as JymlAST.Cons)) {
                    if (((c.cdr as JymlAST.Cons).cdr as JymlAST.Cons).car is string atom) {
                        return JymlType.CreateType(atom);  // 普通变量或 lambda 变量
                    }
                    else if (((c.cdr as JymlAST.Cons).cdr as JymlAST.Cons).car is JymlAST.Cons cons) {
                        return JymlType.CreateType(Eval(cons, env).car);    // 返回一个 value 的子表达式（过程调用）
                    }
                    else {
                        throw new Exception($"define 表达式 {exp} 的 value 部分只能是一个原子值或一个返回原子值的过程调用。");
                    }
                }
                else {
                    return new Procedures(Parser.GetProcedureName(exp), (JymlAST.Cons)Parser.GetLambdaParameters(exp), Parser.GetLambdaBody(exp), env);   // 过程定义
                }
            }

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
        private static JymlAST.Cons EvalSequence(JymlAST.Cons cons, JymlEnvironment env) {
            if (cons.cdr == null) {
                return Eval(cons.car as JymlAST.Cons, env);
            }
            else {
                return EvalSequence(cons.cdr as JymlAST.Cons, env);
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
        private static JymlAST.Cons Apply(JymlAST.Cons proc, JymlAST.Cons arguments, JymlEnvironment env) {
            if (proc.car is string procedureName) {
                if (PrimitiveProcedure.PrimitiveProcedures.Keys.Contains(procedureName)) {
                    // (apply-primitive-procedure procedure arguments)
                    return new JymlAST.Cons(PrimitiveProcedure.PrimitiveProcedures[procedureName].Invoke(arguments.ParametersToArguments(env)));
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
                    JymlType[] values = arguments.ParametersToArguments(env);
                    return EvalSequence(
                        cons: proc,
                        env: p.Environment.ExtendEnvironment(variables, values)
                    );
                }
            }
            else {
                throw new Exception();
            }
        }

        private static JymlAST.Cons Apply(string proc, JymlAST.Cons arguments, JymlEnvironment env) {
            if (PrimitiveProcedure.PrimitiveProcedures.Keys.Contains(proc)) {
                // (apply-primitive-procedure procedure arguments)
                return new JymlAST.Cons(PrimitiveProcedure.PrimitiveProcedures[proc].Invoke(arguments.ParametersToArguments(env)));
            }
            else {
                /*
                 * (eval-sequence (procedure-body procedure) (extend-enviroment
                                                        (procedure-parameters procedure)
                                                        arguments
                                                        (procedure-enviroment procedure)))
                 */
                Procedures p = env.FrameNode[proc].Value as Procedures;
                string[] variables = p.Parameters.ToArray<string>();
                JymlType[] values = arguments.ParametersToArguments(env);
                //return EvalSequence(
                //    cons: (JymlAST.Cons)proc,
                //    env: p.Environment.ExtendEnvironment(variables, values)
                //);
                return Eval(p.Body, p.Environment.ExtendEnvironment(variables, values));
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
        private static JymlAST.Cons ListOfValues(JymlAST.Cons arguments, JymlEnvironment env) {
            // 考虑实现下面的 Select
            //arguments.Select(o => {
            //    if (o is string str) {
            //        return str;
            //    }
            //    else if (o is JymlAST.Cons cons) {
            //        return Eval(cons, env);
            //    }
            //});
            if (arguments != null) {
                if (arguments.car is string arg) {
                    return new JymlAST.Cons(arg, ListOfValues(arguments.cdr as JymlAST.Cons, env));
                }
                else {
                    return new JymlAST.Cons(Eval(arguments.car as JymlAST.Cons, env).car, ListOfValues(arguments.cdr as JymlAST.Cons, env));
                }
            }
            else {
                return null;
            }
        }
    }
}
