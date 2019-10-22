using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using JymlEnvironment;

namespace JymlTypeSystem {
    public abstract class JymlType {
        public static JymlType CreateType(string str) {
            throw new NotImplementedException();
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
        public override string ToString() {
            throw new NotImplementedException();
        }
    }

    public class String : JymlType {
        public override string ToString() {
            throw new NotImplementedException();
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
}
