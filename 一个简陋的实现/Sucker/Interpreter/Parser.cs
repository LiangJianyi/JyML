using JymlAST;
using System;
using System.Linq;
using System.Collections.Generic;

/*
 (define (parse tokens)
  (define list-begin-marks (list #\( #\[))
  (define list-end-marks (list #\) #\]))
  (define (f)
    (if [null? tokens]
        null
        [cond [(list? (member [car tokens] list-begin-marks))
               (set! tokens [cdr tokens])
               (cons (f) (f))]
              [(list? (member [car tokens] list-end-marks))
               (set! tokens [cdr tokens])
               null]
              [else
               (cons [car tokens]
                     (begin
                       (set! tokens [cdr tokens])
                       (f)))]]))
  (f))
	 */

namespace JymlParser {
    public enum Language {
        SuckerML,
        SuckerScript
    }

    public enum Keyword {
        Define,
        Cons,
        If,
        Begin,
        Lambda,
        Set
    }

    public static class Parser {
        private static readonly List<string> _beginMarks = new List<string>() { "(", "[" };

        private static readonly List<string> _endMarks = new List<string>() { ")", "]" };

        private static readonly Dictionary<Keyword, string> _keywords = new Dictionary<Keyword, string>() {
            { Keyword.Define, "define" },
            { Keyword.Cons, "cons" },
            { Keyword.If, "if" },
            { Keyword.Begin, "begin" },
            { Keyword.Lambda, "lambda" },
            { Keyword.Set, "set" }
        };

        private static Cons GenerateAst() {
            if (_tokens == null) {
                return null;
            }
            else {
                string token = _tokens.car as string;
                if (_beginMarks.Contains(token)) {
                    _tokens = _tokens.cdr as Cons;
                    return new Cons(GenerateAst(), GenerateAst());
                }
                else if (_endMarks.Contains(token)) {
                    _tokens = _tokens.cdr as Cons;
                    return null;
                }
                else {
                    _tokens = _tokens.cdr as Cons;
                    return new Cons(token, GenerateAst());
                }
            }
        }

        private static Cons _tokens;

        public static bool IsLambda(Cons ast) => GetTagOfList(ast, _keywords[Keyword.Lambda]);

        public static Cons GetLambdaBody(Cons ast) => (ast.cdr as Cons).cdr as Cons;

        public static (Language lang, Cons ast) GenerateAst(string text) {
            Tokenizer tokenizer = new Tokenizer(text: text,
                                                seperators: new char[] { ' ', '\n' },
                                                singles: new char[] { '(', ')', '[', ']' },
                                                whiteList: new Tokenizer.SelfExistentToken('"', '"'));
            tokenizer.CleanUpTokens();
            _tokens = Cons.FromArray(tokenizer.Tokens);
            if (_tokens.car as string == "#lang") {
                string lang = (_tokens.cdr as Cons).car as string;
                _tokens = (_tokens.cdr as Cons).cdr as Cons;
                if (lang == "SuckerML") {
                    // 调用 SuckerML 解释器
                    return (Language.SuckerML, GenerateAst());
                }
                else if (lang == "SuckerScript") {
                    // 调用 SuckerScript 解释器
                    return (Language.SuckerScript, GenerateAst());
                }
                else {
                    throw new FormatException($"语言指示符不存在：#lang {lang}");
                }
            }
            else {
                throw new FormatException("缺失语言指示符 #lang");
            }
        }

        public static bool IsBegin(Cons ast) => GetTagOfList(ast, _keywords[Keyword.Begin]);

        public static bool IsIf(Cons ast) => GetTagOfList(ast, _keywords[Keyword.If]);

        public static bool IsDefinition(Cons ast) => GetTagOfList(ast, _keywords[Keyword.Define]);

        public static bool IsCompoundProcedure(Cons ast) {
            if (IsDefinition(ast)) {
                ast = ast.cdr as Cons;
                return ast.car is Cons && ast.cdr is Cons;
            }
            else {
                return false;
            }
        }

        public static string[] GetLambdaParameters(Cons ast) {
            Cons parameters = ((ast.cdr as Cons).car as Cons).cdr as Cons;
            return parameters.ToArray<string>();
        }

        public static bool IsAssignment(Cons ast) => GetTagOfList(ast, _keywords[Keyword.Set]);

        public static bool GetTagOfList(Cons ast, string tag) {
            if (ast.car is string str) {
                return str == tag;
            }
            else {
                return false;
            }
        }

        public static bool IsVariable(Cons ast) {
            if (ast.car is string str) {
                /*
                 * 变量名只能是字母、$ 或下划线开头
                 */
                if (char.IsLetter(str[0])) {
                    return true;
                }
                else if (str[0] == '$') {
                    return true;
                }
                else if (str[0] == '_') {
                    return true;
                }
                else {
                    return false;
                }
            }
            else {
                return false;
            }
        }

        public static bool IsSelfEvaluating(Cons ast) {
            /*
             * (cond   [(number? exp) true]
                [(string? exp) true]
                [else false]))
             */
            if (ast.car is string str) {
                try {
                    JymlTypeSystem.JymlType.CreateType(str);
                    return true;
                }
                catch (InvalidCastException) {
                    return false;
                }
            }
            else {
                return false;
            }
        }

        public static string GetProcedureName(Cons exp) {
            if ((exp.cdr as Cons).car is Cons proc) {
                return proc.car as string;
            }
            else if ((exp.cdr as Cons).car is string var) {
                if (((exp.cdr as Cons).cdr as Cons).car is Cons lambdaExp) {
                    if (IsLambda(lambdaExp)) {
                        return var;
                    }
                    else {
                        throw new Exception($"变量 {var} 必须接收一个 lambda 表达式。");
                    }
                }
                else {
                    throw new Exception($"表达式 {exp} 不是一个过程定义。");
                }
            }
            else {
                throw new Exception($"表达式 {exp} 不是一个过程定义。");
            }
        }
    }
}
