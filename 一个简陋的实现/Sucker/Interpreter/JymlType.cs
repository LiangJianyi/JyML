using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using JymlEnvironment;

namespace JymlTypeSystem {
    interface IPrimitiveProcedure {
        BigInteger Add(BigInteger x, BigInteger y);
        BigInteger Add(BigInteger x, BigInteger y);
        BigInteger Add(BigInteger x, BigInteger y);
        BigInteger Add(BigInteger x, BigInteger y);
    }
    public abstract class JymlType {
        public static readonly Dictionary<string, PrimitiveProcedure> _primitiveProcedures =
            new Dictionary<string, PrimitiveProcedure>(){
                { "Add", new PrimitiveProcedure("Add",(BigInteger x, BigInteger y) => x + y) },
                { "Add", new PrimitiveProcedure("Add",(BigInteger x, DateTime y) => x + y) },
                { "Add", new PrimitiveProcedure("Add",(DateTime x, BigInteger y) => x + y) },
                { "Add", new PrimitiveProcedure("Add",(DateTime x, DateTime y) => x + y) },
                { "Sub", new PrimitiveProcedure("Sub",(BigInteger x, BigInteger y) => x - y) },
                { "Sub", new PrimitiveProcedure("Sub",(BigInteger x, DateTime y) => x - y) },
                { "Sub", new PrimitiveProcedure("Sub",(DateTime x, BigInteger y) => x - y) },
                { "Sub", new PrimitiveProcedure("Sub",(DateTime x, DateTime y) => x - y) },
                { "Multi", new PrimitiveProcedure("Multi",(BigInteger x, BigInteger y) => x * y) },
                { "Div", new PrimitiveProcedure("Div",(BigInteger x, BigInteger y) => x / y) },
                { "Rem", new PrimitiveProcedure("Rem",(BigInteger x, BigInteger y) => x % y) },
                { "Cons", new PrimitiveProcedure("Cons",(JymlType x) => new JymlAST.Cons(x)) },
                { "Cons", new PrimitiveProcedure("Cons",(JymlType x, JymlType y) => new JymlAST.Cons(x,y)) },
            };

        public static JymlType CreateType(string str) {
            throw new NotImplementedException();
        }
    }

    public class Null : JymlType { }

    public class Boolean : JymlType {
        private bool _bool;

        public Boolean(bool b) => _bool = b;

        public Boolean(string exp) {
            switch (exp.ToLower()) {
                case "true":
                    _bool = true;
                    break;
                case "false":
                    _bool = false;
                    break;
                default:
                    throw new ArithmeticException("无效的 token。");
            }
        }

        public override string ToString() {
            if (_bool) {
                return "true";
            }
            else {
                return "false";
            }
        }
    }

    public class Number : JymlType {
        private BigInteger _number;

        public Number(string exp) => this._number = BigInteger.Parse(exp);

        public override string ToString() => _number.ToString();
    }

    public class String : JymlType {
        private string _string;

        public String(string exp) {
            if (exp[0] == '"' && exp[exp.Length - 1] == '"') {
                _string = exp.Substring(exp.Length - 1, 1).Substring(0, 1);
            }
            else {
                throw new Exception("字符串缺乏双引号");
            }
        }

        public override string ToString() {
            return _string;
        }
    }

    public class DateTime : JymlType {
        public System.DateTime Date { get; private set; }

        public DateTime(string exp) {
            string[] tokens = exp.Split('/');
            if (tokens.Length == 3) {
                int monthValue = Convert.ToInt32(tokens[0]);
                int dayValue = Convert.ToInt32(tokens[1]);
                int yearValue = Convert.ToInt32(tokens[2]);
                Date = new System.DateTime(yearValue, monthValue, dayValue);
            }
            else {
                throw new ArgumentOutOfRangeException($"Date time format error: {tokens}");
            }
        }

        public override string ToString() {
            return $"{Date.Month}/{Date.Day}/{Date.Year}";
        }
    }

    /// <summary>
    /// 代表解释器的过程
    /// </summary>
    public class Procedures : JymlType {
        private readonly string _name;
        private readonly JymlEnviroment[] _arguments;
        private readonly JymlAST.Cons _body;

        private string GenerateName() {
            Random random = new Random();
            const string CHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
            return "Lambda_" + new string(Enumerable.Repeat(CHARS, 8).Select(s => s[random.Next(s.Length)]).ToArray());
        }

        public override string ToString() {
            throw new NotImplementedException();
        }
    }

    public class PrimitiveProcedure : JymlType {
        public string Name { get; private set; }
        public Func<BigInteger, BigInteger, BigInteger> Proc { get; private set; }
        public object Result { get; private set; }

        public PrimitiveProcedure(string name, Func<BigInteger, BigInteger, BigInteger> proc) {
            Name = name;
            Proc = proc;
        }

        public PrimitiveProcedure(string exp) {
            // Generate method by exp
            if (true) {
                ((string ArgumentTypeToke, string ArgumentContentToken)[] ArgumentTokens, string ReturnTypeToken) arguments = ParseArguments(exp);
                string code = $"static {arguments.ReturnTypeToken} F({ParseArgumentTypeToken(arguments.ArgumentTokens)}) " +
                              $"{{" +
                              $"}}" +
                              $"f({ParseArgumentContentToken(arguments.ArgumentTokens)})";

            }
            else if (true) {

            }
            else if (true) {

            }
        }

        private ((string ArgumentTypeToke, string ArgumentContentToken)[] ArgumentTokens, string ReturnTypeToken) ParseArguments(string exp) {
            /*
             * exp BNF specifition
             * "<ReturnType>::<ProcName>(<ParametersList>)"
                ParametersList::="[<JymlType>::<Value>] | {<JymlType>::<Value>[,<JymlType>::<Value>]}"
                ReturnType::="void|<JymlType>"
             */
            ((string ArgumentTypeToke, string ArgumentContentToken)[] ArgumentTokens, string ReturnTypeToken) res;
            const char COLON = ':';
            exp = exp.Trim();
            int firstColonIndex = exp.IndexOf(COLON);
            if (firstColonIndex > 1 && exp[firstColonIndex + 1] == COLON) {
                res.ReturnTypeToken = exp.Substring(0, firstColonIndex + 1);
                try {
                    exp = exp.Substring(exp.IndexOf('(')); // 截取 (<ParametersList>)
                }
                catch (ArgumentOutOfRangeException ex) {
                    throw new Exception("参数列表缺失圆括号：‘(’", ex);
                }
                try {
                    exp = exp.Substring(1, exp.IndexOf(')') - 1);   // 删除')'
                }
                catch (ArgumentOutOfRangeException ex) {
                    throw new Exception("参数列表缺失圆括号：‘)’", ex);
                }
                try {
                    exp = exp.Substring(exp.IndexOf('(') + 1);   // 删除'('
                }
                catch (ArgumentOutOfRangeException ex) {
                    throw new Exception("参数列表缺失圆括号：‘(’", ex);
                }
            }
            else {
                throw new Exception("表达式缺失返回值。");
            }
        }

        private string ParseArgumentContentToken((string ArgumentTypeToke, string ArgumentContentToken)[] argumentTokens) {
            throw new NotImplementedException();
        }

        private string ParseArgumentTypeToken((string ArgumentTypeToke, string ArgumentContentToken)[] argumentTokens) {
            throw new NotImplementedException();
        }

        public object Invoke() => Result;
    }
}
