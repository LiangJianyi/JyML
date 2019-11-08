using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using Janyee.Utilty;
using Jyml.Environment;

namespace JymlTypeSystem {
    public abstract class JymlType {
        public static JymlType CreateType(string str) {
            try {
                return new Null(str);
            }
            catch (InvalidCastException) {
                try {
                    return new Boolean(str);
                }
                catch (InvalidCastException) {
                    try {
                        return new Number(str);
                    }
                    catch (InvalidCastException) {
                        try {
                            return new String(str);
                        }
                        catch (InvalidCastException) {
                            try {
                                return new DateTime(str);
                            }
                            catch (InvalidCastException) {
                                // 思考一下将 str 转换为 procedure
                                throw new InvalidCastException($"{str} 无法转换为任何类型的 JymlType。");
                            }
                        }
                    }
                }
            }
        }

        public static JymlType CreateType(object obj) {
            try {
                return new Null(obj);
            }
            catch (InvalidCastException) {
                try {
                    return new Boolean(obj);
                }
                catch (InvalidCastException) {
                    try {
                        return new Number(obj);
                    }
                    catch (InvalidCastException) {
                        try {
                            return new String(obj);
                        }
                        catch (InvalidCastException) {
                            try {
                                return new DateTime(obj);
                            }
                            catch (InvalidCastException) {
                                try {
                                    return new Cons(obj);
                                }
                                catch (InvalidCastException ex) {
                                    throw new InvalidCastException($"{obj} 无法转换为任何类型的 JymlType。{ex.Message}");
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    public class Null : JymlType {
        public Null(string str) {
            if (str != "null") {
                throw new InvalidCastException($"无法解析 {str}, 空类型应该为：null.");
            }
        }

        public Null(object obj) {
            if (!(obj is Null)) {
                throw new InvalidCastException($"对象 {obj} 与 类型 Null 不匹配，其类型为：{obj.GetType()}。");
            }
        }
    }

    public class Boolean : JymlType {
        internal bool _bool;

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
                    throw new InvalidCastException("无效的 token。");
            }
        }

        public Boolean(object obj) {
            if (obj is Boolean b) {
                _bool = b;
            }
            else {
                throw new InvalidCastException($"对象 {obj} 与 类型 Boolean 不匹配，其类型为：{obj.GetType()}。");
            }
        }

        public static Boolean operator !(Boolean other) => new Boolean(!other._bool);
        public static Boolean operator ==(Boolean b1, Boolean b2) => new Boolean(b1._bool == b2._bool);
        public static Boolean operator !=(Boolean b1, Boolean b2) => new Boolean(b1._bool != b2._bool);
        public static implicit operator bool(Boolean b) => b._bool;

        public static Boolean And(Boolean b1, Boolean b2) => new Boolean(b1._bool && b2._bool);
        public static Boolean Or(Boolean b1, Boolean b2) => new Boolean(b1._bool || b2._bool);
        public static Boolean Not(Boolean b) => !b;

        public override string ToString() {
            if (_bool) {
                return "true";
            }
            else {
                return "false";
            }
        }

        public override int GetHashCode() {
            unchecked {
                return 17 * 23 + _bool.GetHashCode();
            }
        }

        public override bool Equals(object obj) => _bool == (obj as Boolean)._bool;
    }

    public class Number : JymlType {
        internal BigInteger _number;

        public Number(string exp) {
            try {
                this._number = BigInteger.Parse(exp);
            }
            catch (FormatException ex) {
                throw new InvalidCastException($"{exp} 是无效的 Number。", ex);
            }
        }

        public Number(BigInteger bi) => _number = bi;

        public Number(object obj) {
            if (obj is Number num) {
                _number = num._number;
            }
            else {
                throw new InvalidCastException($"对象 {obj} 与 类型 Number 不匹配，其类型为：{obj.GetType()}。");
            }
        }

        public static Number operator +(Number x, Number y) => new Number(x._number + y._number);
        public static Number operator -(Number x, Number y) => new Number(x._number - y._number);
        public static Number operator *(Number x, Number y) => new Number(x._number * y._number);
        public static Number operator /(Number x, Number y) => new Number(x._number / y._number);
        public static Number operator %(Number x, Number y) => new Number(x._number % y._number);
        public static Boolean operator <(Number left, Number right) => new Boolean(left._number < right._number);
        public static Boolean operator <=(Number left, Number right) => new Boolean(left._number <= right._number);
        public static Boolean operator ==(Number left, Number right) => new Boolean(left._number == right._number);
        public static Boolean operator !=(Number left, Number right) => new Boolean(left._number != right._number);
        public static Boolean operator >=(Number left, Number right) => new Boolean(left._number >= right._number);
        public static Boolean operator >(Number left, Number right) => new Boolean(left._number > right._number);

        public override string ToString() => _number.ToString();

        public override int GetHashCode() {
            unchecked {
                return 17 * 23 + _number.GetHashCode();
            }
        }

        public override bool Equals(object obj) => _number == (obj as Number)._number;
    }

    public class String : JymlType {
        internal string _string;

        public String(string exp) {
            if (exp[0] == '"' && exp[exp.Length - 1] == '"') {
                if (exp[0] == '"' && exp[1] == '"') {
                    _string = exp;
                }
                else {
                    _string = exp.Substring(1, exp.Length - 2);
                }
            }
            else {
                throw new InvalidCastException("字符串缺乏双引号");
            }
        }

        public String(object obj) {
            if (obj is String str) {
                _string = str._string;
            }
            else {
                throw new InvalidCastException($"对象 {obj} 与 类型 String 不匹配，其类型为：{obj.GetType()}。");
            }
        }

        public static Boolean operator ==(String s1, String s2) => new Boolean(s1._string == s2._string);
        public static Boolean operator !=(String s1, String s2) => new Boolean(s1._string != s2._string);

        public override string ToString() => $"\"{_string}\"";

        public override int GetHashCode() {
            unchecked {
                return 17 * 23 + _string.GetHashCode();
            }
        }

        public override bool Equals(object obj) => _string == (obj as String)._string;
    }

    public class DateTime : JymlType {
        internal System.DateTime _date;

        public DateTime(string exp) {
            string[] tokens = exp.Split('/');
            if (tokens.Length == 3) {
                int monthValue = Convert.ToInt32(tokens[0]);
                int dayValue = Convert.ToInt32(tokens[1]);
                int yearValue = Convert.ToInt32(tokens[2]);
                _date = new System.DateTime(yearValue, monthValue, dayValue);
            }
            else {
                throw new InvalidCastException($"Date time format error: {tokens}");
            }
        }

        public DateTime(System.DateTime dateTime) => _date = dateTime;

        public DateTime(object obj) {
            if (obj is DateTime dateTime) {
                _date = dateTime._date;
            }
            else {
                throw new InvalidCastException($"对象 {obj} 与 类型 DateTime 不匹配，其类型为：{obj.GetType()}。");
            }
        }

        public static DateTime operator +(Number bi, DateTime dt) => new DateTime(dt._date.AddDays(bi._number.BigIntegerToInt64()));
        public static DateTime operator +(DateTime dt, Number bi) => new DateTime(dt._date.AddDays(bi._number.BigIntegerToInt64()));
        public static Boolean operator ==(DateTime dt1, DateTime dt2) => new Boolean(System.DateTime.Equals(dt1._date, dt2._date));
        public static Boolean operator !=(DateTime dt1, DateTime dt2) => new Boolean(!System.DateTime.Equals(dt1._date, dt2._date));
        public static Boolean operator <(DateTime dt1, DateTime dt2) => new Boolean(System.DateTime.Compare(dt1._date, dt2._date) == -1);
        public static Boolean operator >(DateTime dt1, DateTime dt2) => new Boolean(System.DateTime.Compare(dt1._date, dt2._date) == 1);
        public static Boolean operator <=(DateTime dt1, DateTime dt2) => new Boolean(System.DateTime.Compare(dt1._date, dt2._date) == -1 || System.DateTime.Compare(dt1._date, dt2._date) == 0);
        public static Boolean operator >=(DateTime dt1, DateTime dt2) => new Boolean(System.DateTime.Compare(dt1._date, dt2._date) == 1 || System.DateTime.Compare(dt1._date, dt2._date) == 0);

        public override string ToString() => $"{_date.Month}/{_date.Day}/{_date.Year}";

        public override int GetHashCode() {
            unchecked {
                return 17 * 23 + _date.GetHashCode();
            }
        }

        public override bool Equals(object obj) => _date == (obj as DateTime)._date;
    }

    /// <summary>
    /// 表示解释器的复合过程
    /// </summary>
    public class Procedures : JymlType {
        private readonly string _name;
        private readonly JymlEnvironment _environment;
        private readonly JymlAST.Cons _body;
        private readonly JymlAST.Cons _parameters;

        public string Name => _name;

        public JymlEnvironment Environment => _environment;

        public JymlAST.Cons Body => _body;

        public JymlAST.Cons Parameters => _parameters;

        private string GenerateName() {
            Random random = new Random();
            const string CHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
            return "Lambda_" + new string(Enumerable.Repeat(CHARS, 8).Select(s => s[random.Next(s.Length)]).ToArray());
        }

        public Procedures(JymlAST.Cons parameters, JymlAST.Cons body, JymlEnvironment environment) {
            _name = GenerateName();
            _parameters = parameters;
            _body = body;
            _environment = environment;
        }

        public Procedures(string name, JymlAST.Cons parameters, JymlAST.Cons body, JymlEnvironment environment) {
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
            add,
            sub,
            multi,
            div,
            rem,
            cons,
            car,
            cdr,
            and,
            or,
            not,
            equalTo,
            notEqualTo,
            greaterThan,
            greaterThanOrEqualTo,
            lessThan,
            lessThanOrEqualTo
        }

        private Primitive _primitive;

        public static Dictionary<string, PrimitiveProcedure> PrimitiveProcedures = new Dictionary<string, PrimitiveProcedure>() {
            { Primitive.add.ToString(), new PrimitiveProcedure(Primitive.add) },
            { Primitive.sub.ToString(), new PrimitiveProcedure(Primitive.sub) },
            { Primitive.multi.ToString(), new PrimitiveProcedure(Primitive.multi) },
            { Primitive.div.ToString(), new PrimitiveProcedure(Primitive.div) },
            { Primitive.rem.ToString(), new PrimitiveProcedure(Primitive.rem) },
            { Primitive.cons.ToString(), new PrimitiveProcedure(Primitive.cons) },
            { Primitive.car.ToString(), new PrimitiveProcedure(Primitive.car) },
            { Primitive.cdr.ToString(), new PrimitiveProcedure(Primitive.cdr) },
            { "+", new PrimitiveProcedure(Primitive.add) },
            { "-", new PrimitiveProcedure(Primitive.sub) },
            { "*", new PrimitiveProcedure(Primitive.multi) },
            { "/", new PrimitiveProcedure(Primitive.div) },
            { "%", new PrimitiveProcedure(Primitive.rem) },
            { "<", new PrimitiveProcedure(Primitive.lessThan) },
            { "<=", new PrimitiveProcedure(Primitive.lessThanOrEqualTo)  },
            { "=", new PrimitiveProcedure(Primitive.equalTo) },
            { "!=", new PrimitiveProcedure(Primitive.notEqualTo) },
            { ">", new PrimitiveProcedure(Primitive.greaterThan) },
            { ">=", new PrimitiveProcedure(Primitive.greaterThanOrEqualTo) },
            { "and", new PrimitiveProcedure(Primitive.and) },
            { "or", new PrimitiveProcedure(Primitive.or) },
            { "not", new PrimitiveProcedure(Primitive.not) },
        };

        public PrimitiveProcedure(Primitive primitive) {
            _primitive = primitive;
        }

        public JymlType Invoke(params object[] arguments) {
            switch (_primitive) {
                case Primitive.add:
                    if (arguments.Length == 2) {
                        if (arguments[0] is Number bigInteger1) {
                            if (arguments[1] is Number bigInteger2) {
                                return bigInteger1 + bigInteger2;
                            }
                            else if (arguments[1] is DateTime dateTime) {
                                return bigInteger1 + dateTime;
                            }
                            else {
                                throw new InvalidCastException($"参数 {arguments[1]} 无法匹配 {_primitive} 过程。");
                            }
                        }
                        else if (arguments[0] is DateTime dateTime) {
                            if (arguments[1] is Number bigInteger) {
                                return dateTime + bigInteger;
                            }
                            else {
                                throw new InvalidCastException($"参数 {arguments[1]} 无法匹配 {_primitive} 过程。");
                            }
                        }
                        else {
                            throw new InvalidCastException($"参数 {arguments[0]} 无法匹配 {_primitive} 过程。");
                        }
                    }
                    else {
                        throw new InvalidProgramException($"{_primitive} 过程是个二元运算符，只能匹配两个运算对象。");
                    }
                case Primitive.sub:
                    if (arguments.Length == 2) {
                        if (arguments[0] is Number bigInteger3) {
                            if (arguments[1] is Number bigInteger4) {
                                return bigInteger3 - bigInteger4;
                            }
                            else {
                                throw new InvalidCastException($"参数 {arguments[1]} 无法匹配 {_primitive} 过程。");
                            }
                        }
                        else {
                            throw new InvalidCastException($"参数 {arguments[0]} 无法匹配 {_primitive} 过程。");
                        }
                    }
                    else {
                        throw new InvalidProgramException($"{_primitive} 过程是个二元运算符，只能匹配两个运算对象。");
                    }
                case Primitive.multi:
                    if (arguments.Length == 2) {
                        if (arguments[0] is Number bigInteger5) {
                            if (arguments[1] is Number bigInteger6) {
                                return bigInteger5 * bigInteger6;
                            }
                            else {
                                throw new InvalidCastException($"参数 {arguments[1]} 无法匹配 {_primitive} 过程。");
                            }
                        }
                        else {
                            throw new InvalidCastException($"参数 {arguments[0]} 无法匹配 {_primitive} 过程。");
                        }
                    }
                    else {
                        throw new InvalidProgramException($"{_primitive} 过程是个二元运算符，只能匹配两个运算对象。");
                    }
                case Primitive.div:
                    if (arguments.Length == 2) {
                        if (arguments[0] is Number bigInteger7) {
                            if (arguments[1] is Number bigInteger8) {
                                return bigInteger7 - bigInteger8;
                            }
                            else {
                                throw new InvalidCastException($"参数 {arguments[1]} 无法匹配 {_primitive} 过程。");
                            }
                        }
                        else {
                            throw new InvalidCastException($"参数 {arguments[0]} 无法匹配 {_primitive} 过程。");
                        }
                    }
                    else {
                        throw new InvalidProgramException($"{_primitive} 过程是个二元运算符，只能匹配两个运算对象。");
                    }
                case Primitive.rem:
                    if (arguments.Length == 2) {
                        if (arguments[0] is Number bigInteger9) {
                            if (arguments[1] is Number bigInteger10) {
                                return bigInteger9 - bigInteger10;
                            }
                            else {
                                throw new InvalidCastException($"参数 {arguments[1]} 无法匹配 {_primitive} 过程。");
                            }
                        }
                        else {
                            throw new InvalidCastException($"参数 {arguments[0]} 无法匹配 {_primitive} 过程。");
                        }
                    }
                    else {
                        throw new InvalidProgramException($"{_primitive} 过程是个二元运算符，只能匹配两个运算对象。");
                    }
                case Primitive.cons:
                    if (arguments.Length == 1) {
                        return (JymlType)new JymlAST.Cons(arguments[0]);
                    }
                    else if (arguments.Length == 2) {
                        return (JymlType)new JymlAST.Cons(arguments[0], arguments[1]);
                    }
                    else if (arguments.Length > 2) {
                        JymlAST.Cons cons = new JymlAST.Cons(null);
                        JymlAST.Cons current = cons;
                        for (int i = 0; i < arguments.Length; i++) {
                            cons.car = arguments[i];
                            if (i < arguments.Length - 1) {
                                cons.cdr = new JymlAST.Cons(null);
                                cons = cons.cdr as JymlAST.Cons;
                            }
                            else {
                                cons.cdr = null;
                            }
                        }
                        return new Cons(current);
                    }
                    else {
                        throw new InvalidCastException($"参数列表与 {_primitive} 不匹配。");
                    }
                case Primitive.car:
                    if (arguments.Length == 1) {
                        if (arguments[0] is Cons cons) {
                            return JymlType.CreateType(cons._cons.car);
                        }
                        else {
                            throw new InvalidCastException($"参数 {arguments[0]} 无法匹配 {_primitive} 过程。");
                        }
                    }
                    else {
                        throw new InvalidCastException($"参数列表与 {_primitive} 不匹配。");
                    }
                case Primitive.cdr:
                    if (arguments.Length == 1) {
                        if (arguments[0] is Cons cons) {
                            return JymlType.CreateType(cons._cons.cdr);
                        }
                        else {
                            throw new InvalidCastException($"参数 {arguments[0]} 无法匹配 {_primitive} 过程。");
                        }
                    }
                    else {
                        throw new InvalidCastException($"参数列表与 {_primitive} 不匹配。");
                    }
                case Primitive.and:
                    if (arguments.Length == 2) {
                        if (arguments[0] is Boolean bool1) {
                            if (arguments[1] is Boolean bool2) {
                                return Boolean.And(bool1, bool2);
                            }
                            else {
                                throw new InvalidCastException($"参数 {arguments[1]} 无法匹配 {_primitive} 过程。");
                            }
                        }
                        else {
                            throw new InvalidCastException($"参数 {arguments[0]} 无法匹配 {_primitive} 过程。");
                        }
                    }
                    else {
                        throw new InvalidProgramException($"{_primitive} 过程是个二元运算符，只能匹配两个运算对象。");
                    }
                case Primitive.or:
                    if (arguments.Length == 2) {
                        if (arguments[0] is Boolean bool1) {
                            if (arguments[1] is Boolean bool2) {
                                return Boolean.Or(bool1, bool2);
                            }
                            else {
                                throw new InvalidCastException($"参数 {arguments[1]} 无法匹配 {_primitive} 过程。");
                            }
                        }
                        else {
                            throw new InvalidCastException($"参数 {arguments[0]} 无法匹配 {_primitive} 过程。");
                        }
                    }
                    else {
                        throw new InvalidProgramException($"{_primitive} 过程是个二元运算符，只能匹配两个运算对象。");
                    }
                case Primitive.not:
                    if (arguments.Length == 1) {
                        if (arguments[0] is Boolean b) {
                            return Boolean.Not(b);
                        }
                        else {
                            throw new InvalidCastException($"参数 {arguments[1]} 无法匹配 {_primitive} 过程。");
                        }
                    }
                    else {
                        throw new InvalidProgramException($"{_primitive} 过程是个二元运算符，只能匹配两个运算对象。");
                    }
                case Primitive.lessThan:
                    if (arguments.Length == 2) {
                        if (arguments[0] is Number number1) {
                            if (arguments[1] is Number number2) {
                                return number1 < number2;
                            }
                            else {
                                throw new InvalidCastException($"参数 {arguments[1]} 无法匹配 {_primitive} 过程。");
                            }
                        }
                        else if (arguments[0] is DateTime date1) {
                            if (arguments[1] is DateTime date2) {
                                return date1 < date2;
                            }
                            else {
                                throw new InvalidCastException($"参数 {arguments[1]} 无法匹配 {_primitive} 过程。");
                            }
                        }
                        else {
                            throw new InvalidCastException($"参数 {arguments[0]} 无法匹配 {_primitive} 过程。");
                        }
                    }
                    else {
                        throw new InvalidProgramException($"{_primitive} 过程是个二元运算符，只能匹配两个运算对象。");
                    }
                case Primitive.lessThanOrEqualTo:
                    if (arguments.Length == 2) {
                        if (arguments[0] is Number number1) {
                            if (arguments[1] is Number number2) {
                                return number1 <= number2;
                            }
                            else {
                                throw new InvalidCastException($"参数 {arguments[1]} 无法匹配 {_primitive} 过程。");
                            }
                        }
                        else if (arguments[0] is DateTime date1) {
                            if (arguments[1] is DateTime date2) {
                                return date1 <= date2;
                            }
                            else {
                                throw new InvalidCastException($"参数 {arguments[1]} 无法匹配 {_primitive} 过程。");
                            }
                        }
                        else {
                            throw new InvalidCastException($"参数 {arguments[0]} 无法匹配 {_primitive} 过程。");
                        }
                    }
                    else {
                        throw new InvalidProgramException($"{_primitive} 过程是个二元运算符，只能匹配两个运算对象。");
                    }
                case Primitive.equalTo:
                    if (arguments.Length == 2) {
                        if (arguments[0] is Number number1) {
                            if (arguments[1] is Number number2) {
                                return number1 == number2;
                            }
                            else {
                                throw new InvalidCastException($"参数 {arguments[1]} 无法匹配 {_primitive} 过程。");
                            }
                        }
                        else if (arguments[0] is DateTime date1) {
                            if (arguments[1] is DateTime date2) {
                                return date1 == date2;
                            }
                            else {
                                throw new InvalidCastException($"参数 {arguments[1]} 无法匹配 {_primitive} 过程。");
                            }
                        }
                        else if (arguments[0] is Boolean bool1) {
                            if (arguments[1] is Boolean bool2) {
                                return bool1 == bool2;
                            }
                            else {
                                throw new InvalidCastException($"参数 {arguments[1]} 无法匹配 {_primitive} 过程。");
                            }
                        }
                        else if (arguments[0] is String str1) {
                            if (arguments[1] is String str2) {
                                return str1 == str2;
                            }
                            else {
                                throw new InvalidCastException($"参数 {arguments[1]} 无法匹配 {_primitive} 过程。");
                            }
                        }
                        else if (arguments[0] is Cons cons1) {
                            if (arguments[1] is Cons cons2) {
                                return cons1 == cons2;
                            }
                            else {
                                throw new InvalidCastException($"参数 {arguments[1]} 无法匹配 {_primitive} 过程。");
                            }
                        }
                        else {
                            throw new InvalidCastException($"参数 {arguments[0]} 无法匹配 {_primitive} 过程。");
                        }
                    }
                    else {
                        throw new InvalidProgramException($"{_primitive} 过程是个二元运算符，只能匹配两个运算对象。");
                    }
                case Primitive.greaterThan:
                    if (arguments.Length == 2) {
                        if (arguments[0] is Number number1) {
                            if (arguments[1] is Number number2) {
                                return number1 > number2;
                            }
                            else {
                                throw new InvalidCastException($"参数 {arguments[1]} 无法匹配 {_primitive} 过程。");
                            }
                        }
                        else if (arguments[0] is DateTime date1) {
                            if (arguments[1] is DateTime date2) {
                                return date1 > date2;
                            }
                            else {
                                throw new InvalidCastException($"参数 {arguments[1]} 无法匹配 {_primitive} 过程。");
                            }
                        }
                        else {
                            throw new InvalidCastException($"参数 {arguments[0]} 无法匹配 {_primitive} 过程。");
                        }
                    }
                    else {
                        throw new InvalidProgramException($"{_primitive} 过程是个二元运算符，只能匹配两个运算对象。");
                    }
                case Primitive.greaterThanOrEqualTo:
                    if (arguments.Length == 2) {
                        if (arguments[0] is Number number1) {
                            if (arguments[1] is Number number2) {
                                return number1 >= number2;
                            }
                            else {
                                throw new InvalidCastException($"参数 {arguments[1]} 无法匹配 {_primitive} 过程。");
                            }
                        }
                        else if (arguments[0] is DateTime date1) {
                            if (arguments[1] is DateTime date2) {
                                return date1 >= date2;
                            }
                            else {
                                throw new InvalidCastException($"参数 {arguments[1]} 无法匹配 {_primitive} 过程。");
                            }
                        }
                        else {
                            throw new InvalidCastException($"参数 {arguments[0]} 无法匹配 {_primitive} 过程。");
                        }
                    }
                    else {
                        throw new InvalidProgramException($"{_primitive} 过程是个二元运算符，只能匹配两个运算对象。");
                    }
                case Primitive.notEqualTo:
                    if (arguments.Length == 1) {
                        if (arguments[0] is Number number1) {
                            if (arguments[1] is Number number2) {
                                return number1 != number2;
                            }
                            else {
                                throw new InvalidCastException($"参数 {arguments[1]} 无法匹配 {_primitive} 过程。");
                            }
                        }
                        else if (arguments[0] is DateTime date1) {
                            if (arguments[1] is DateTime date2) {
                                return date1 != date2;
                            }
                            else {
                                throw new InvalidCastException($"参数 {arguments[1]} 无法匹配 {_primitive} 过程。");
                            }
                        }
                        else if (arguments[0] is Boolean bool1) {
                            if (arguments[1] is Boolean bool2) {
                                return bool1 != bool2;
                            }
                            else {
                                throw new InvalidCastException($"参数 {arguments[1]} 无法匹配 {_primitive} 过程。");
                            }
                        }
                        else if (arguments[0] is String str1) {
                            if (arguments[1] is String str2) {
                                return str1 != str2;
                            }
                            else {
                                throw new InvalidCastException($"参数 {arguments[1]} 无法匹配 {_primitive} 过程。");
                            }
                        }
                        else {
                            throw new InvalidCastException($"参数 {arguments[0]} 无法匹配 {_primitive} 过程。");
                        }
                    }
                    else {
                        throw new InvalidProgramException($"{_primitive} 过程是个一元运算符，只能匹配一个运算对象。");
                    }
                default:
                    throw new InvalidCastException($"未知过程类型：Primitive.{_primitive}");
            }
        }

        public override string ToString() => $"#<Procedure: {_primitive}>";
    }

    public class Cons : JymlType {
        internal JymlAST.Cons _cons;
        public Cons(JymlAST.Cons cons) => _cons = cons;
        public Cons(object obj) {
            if (obj is Cons cons) {
                _cons = cons._cons;
            }
            else {
                throw new InvalidCastException($"对象 {obj} 与 类型 Cons 不匹配，其类型为：{obj.GetType()}。");
            }
        }
        public override string ToString() => _cons.ToString();

        public static Boolean operator ==(Cons cons1, Cons cons2) => new Boolean(JymlAST.Cons.Equals(cons1, cons2));
        public static Boolean operator !=(Cons cons1, Cons cons2) => new Boolean(!JymlAST.Cons.Equals(cons1, cons2));

        public override bool Equals(object obj) => this == obj as Cons;

        public override int GetHashCode() {
            unchecked {
                return 17 * 23 + _cons.GetHashCode();
            }
        }
    }
}
