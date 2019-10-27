using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using Janyee.Utilty;
using JymlAST;

namespace JymlTypeSystem {
    public abstract class JymlType {
        public static JymlType CreateType(string str) {
            try {
                return new Null(str);
            }
            catch (Exception) {
                try {
                    return new Boolean(str);
                }
                catch (Exception) {
                    try {
                        return new Number(str);
                    }
                    catch (Exception) {
                        try {
                            return new String(str);
                        }
                        catch (Exception) {
                            try {
                                return new DateTime(str);
                            }
                            catch (Exception) {
                                // 思考一下将 str 转换为 procedure
                                throw;
                            }
                        }
                    }
                }
            }
        }
    }

    public class Null : JymlType {
        public Null(string str) {
            if (str!="null") {
                throw new Exception($"无法解析 {str}, 空类型应该为：null.");
            }
        }
    }

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
                    throw new Exception("无效的 token。");
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
                throw new Exception($"Date time format error: {tokens}");
            }
        }

        public DateTime(System.DateTime dateTime) => Date = dateTime;

        public static DateTime operator +(BigInteger bi, DateTime dt) => new DateTime(dt.Date.AddDays(bi.BigIntegerToInt64()));
        public static DateTime operator +(DateTime dt, BigInteger bi) => new DateTime(dt.Date.AddDays(bi.BigIntegerToInt64()));

        public override string ToString() {
            return $"{Date.Month}/{Date.Day}/{Date.Year}";
        }
    }

    /// <summary>
    /// 表示解释器的复合过程
    /// </summary>
    public class Procedures : JymlType {
        private readonly string _name;
        private readonly JymlEnvironment.JymlEnvironment _environment;
        private readonly Cons _body;
        private readonly Cons _parameters;

        public string Name => _name;

        public JymlEnvironment.JymlEnvironment Environment => _environment;

        public Cons Body => _body;

        public Cons Parameters => _parameters;

        private string GenerateName() {
            Random random = new Random();
            const string CHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
            return "Lambda_" + new string(Enumerable.Repeat(CHARS, 8).Select(s => s[random.Next(s.Length)]).ToArray());
        }

        public Procedures(Cons parameters, Cons body, JymlEnvironment.JymlEnvironment environment) {
            _name = GenerateName();
            _parameters = parameters;
            _body = body;
            _environment = environment;
        }

        public Procedures(string name, Cons parameters, Cons body, JymlEnvironment.JymlEnvironment environment) {
            _name = name;
            _parameters = parameters;
            _body = body;
            _environment = environment;
        }

        public override string ToString() {
            return $"#<{_name}>_<{_parameters}";
        }
    }

    /// <summary>
    /// 表示解释器的基本过程
    /// </summary>
    public class PrimitiveProcedure : JymlType {
        /// <summary>
        /// 表示基本过程的名称
        /// </summary>
        public enum Primitive {
            Add,
            Sub,
            Multi,
            Div,
            Rem,
            Cons
        }

        private Primitive _primitive;

        public static Dictionary<string, PrimitiveProcedure> PrimitiveProcedures = new Dictionary<string, PrimitiveProcedure>() {
            { Primitive.Add.ToString(), new PrimitiveProcedure(Primitive.Add) },
            { Primitive.Sub.ToString(), new PrimitiveProcedure(Primitive.Sub) },
            { Primitive.Multi.ToString(), new PrimitiveProcedure(Primitive.Multi) },
            { Primitive.Div.ToString(), new PrimitiveProcedure(Primitive.Div) },
            { Primitive.Rem.ToString(), new PrimitiveProcedure(Primitive.Rem) },
            { Primitive.Cons.ToString(), new PrimitiveProcedure(Primitive.Cons) },
        };

        public PrimitiveProcedure(Primitive primitive) {
            _primitive = primitive;
        }

        public object Invoke(params object[] arguments) {
            switch (_primitive) {
                case Primitive.Add:
                    if (arguments[0] is BigInteger bigInteger1) {
                        if (arguments[1] is BigInteger bigInteger2) {
                            return bigInteger1 + bigInteger2;
                        }
                        else if (arguments[1] is DateTime dateTime) {
                            return bigInteger1 + dateTime;
                        }
                        else {
                            throw new Exception($"参数 {arguments[1]} 无法匹配 {_primitive} 方法。");
                        }
                    }
                    else if (arguments[0] is DateTime dateTime) {
                        if (arguments[1] is BigInteger bigInteger) {
                            return dateTime + bigInteger;
                        }
                        else {
                            throw new Exception($"参数 {arguments[1]} 无法匹配 {_primitive} 方法。");
                        }
                    }
                    else {
                        throw new Exception($"参数 {arguments[0]} 无法匹配 {_primitive} 方法。");
                    }
                case Primitive.Sub:
                    if (arguments[0] is BigInteger bigInteger3) {
                        if (arguments[1] is BigInteger bigInteger4) {
                            return bigInteger3 - bigInteger4;
                        }
                        else {
                            throw new Exception($"参数 {arguments[1]} 无法匹配 {_primitive} 方法。");
                        }
                    }
                    else {
                        throw new Exception($"参数 {arguments[0]} 无法匹配 {_primitive} 方法。");
                    }
                case Primitive.Multi:
                    if (arguments[0] is BigInteger bigInteger5) {
                        if (arguments[1] is BigInteger bigInteger6) {
                            return bigInteger5 * bigInteger6;
                        }
                        else {
                            throw new Exception($"参数 {arguments[1]} 无法匹配 {_primitive} 方法。");
                        }
                    }
                    else {
                        throw new Exception($"参数 {arguments[0]} 无法匹配 {_primitive} 方法。");
                    }
                case Primitive.Div:
                    if (arguments[0] is BigInteger bigInteger7) {
                        if (arguments[1] is BigInteger bigInteger8) {
                            return bigInteger7 - bigInteger8;
                        }
                        else {
                            throw new Exception($"参数 {arguments[1]} 无法匹配 {_primitive} 方法。");
                        }
                    }
                    else {
                        throw new Exception($"参数 {arguments[0]} 无法匹配 {_primitive} 方法。");
                    }
                case Primitive.Rem:
                    if (arguments[0] is BigInteger bigInteger9) {
                        if (arguments[1] is BigInteger bigInteger10) {
                            return bigInteger9 - bigInteger10;
                        }
                        else {
                            throw new Exception($"参数 {arguments[1]} 无法匹配 {_primitive} 方法。");
                        }
                    }
                    else {
                        throw new Exception($"参数 {arguments[0]} 无法匹配 {_primitive} 方法。");
                    }
                case Primitive.Cons:
                    if (arguments.Length == 1) {
                        return new Cons(arguments[0]);
                    }
                    else if (arguments.Length > 1) {
                        return new Cons(arguments[0], arguments[1]);
                    }
                    else {
                        throw new Exception($"参数列表与 Cons 不匹配。");
                    }
                default:
                    throw new Exception($"未知过程类型：Primitive.{_primitive}");
            }
        }
    }
}
