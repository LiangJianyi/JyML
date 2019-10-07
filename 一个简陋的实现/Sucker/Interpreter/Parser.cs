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
    public static class Parser {
        private static readonly List<string> _beginMarks = new List<string>() { "(", "[" };

        private static readonly List<string> _endMarks = new List<string>() { ")", "]" };

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

        public static bool IsLambda(Cons ast) => GetTagOfList(ast, "lambda");

        public static Cons GetLambdaBody(Cons ast) => (ast.cdr as Cons).cdr as Cons;

        public static Cons GenerateAst(string text) {
            Tokenizer tokenizer = new Tokenizer(text, new char[] { ' ', '\n' }, new char[] { '(', ')', '[', ']' });
            tokenizer.CleanUpTokens();
            _tokens = Cons.FromArray(tokenizer.Tokens);
            if (_tokens.car as string == "#lang") {
                string lang = (_tokens.cdr as Cons).car as string;
                _tokens = (_tokens.cdr as Cons).cdr as Cons;
                if (lang == "SuckerML") {
                    // 调用 SuckerML 解释器
                    return GenerateAst();
                }
                else if (lang == "SuckerScript") {
                    // 调用 SuckerScript 解释器
                    return GenerateAst();
                }
                else {
                    throw new FormatException($"语言指示符不存在：{lang}");
                }
            }
            else {
                throw new FormatException("缺失语言指示符 #lang");
            }
        }

        public static bool IsBegin(Cons ast) => GetTagOfList(ast, "begin");

        public static bool IsIf(Cons ast) => GetTagOfList(ast, "if");

        public static bool IsDefinition(Cons ast) => GetTagOfList(ast, "define");

        public static bool IsCompoundProcedure(Cons ast) {
            if (IsDefinition(ast)) {
                ast = ast.cdr as Cons;
                return ast.car is Cons && ast.cdr is Cons;
            }
            else {
                return false;
            }
        }

        public static object[] GetLambdaParameters(Cons ast) {
            Cons parameters = (ast.cdr as Cons).car as Cons;
            return parameters.ToArray();
        }

        public static bool IsAssignment(Cons ast) => GetTagOfList(ast, "set");

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

    }
}
