using System;
using System.Collections;
using System.Collections.Generic;

namespace JymlAST {
    public class Cons : IEnumerable<object> {
        public object car;
        public object cdr;

        Cons() : this(null) { }

        public Cons(object car) : this(car, null) { }

        public Cons(object car, object cdr) {
            this.car = car;
            this.cdr = cdr;
        }

        public static Cons FromList(IEnumerable list) {
            if (list == null) {
                return null;
            }
            Cons first = null;
            Cons c = null;
            foreach (object var in list) {
                if (c == null) {
                    first = c = new Cons(var);
                }
                else {
                    Cons d = new Cons(var);
                    c.cdr = d;
                    c = d;
                }
            }
            return first;
        }

        public static Cons FromArray(params object[] args) {
            if (args == null || args.Length == 0) {
                return null;
            }
            else if (args.Length == 1) {
                return new Cons(args[0]);
            }
            else {
                return Cons.FromList(args);
            }
        }

        internal int Length {
            get {
                int i = 0;
                foreach (var o in this) {
                    i++;
                }
                return i;
            }
        }

        public static explicit operator JymlTypeSystem.JymlType(Cons cons) {
            if (cons.car is string str) {
                return JymlTypeSystem.JymlType.CreateType(str);
            }
            else {
                throw new InvalidCastException($"无法将 {cons} 转换为 JymlType 类型");
            }
        }

        public static explicit operator Cons(string s) => new Cons(s);

        public static explicit operator Cons(string[] s) {
            Cons c = new Cons();
            Cons temp = c;
            foreach (var item in s) {
                temp.car = item;
                temp.cdr = new Cons();
                temp = temp.cdr as Cons;
            }
            return c;
        }

        // this only works with proper lists
        public IEnumerator<object> GetEnumerator() {
            Cons current = this;
            while (current != null) {
                yield return current.car;
                current = current.cdr as Cons;
            }
            yield break;
        }

        IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();

        public override string ToString() => $"({this.car} {this.cdr})";

        public override bool Equals(object obj) {
            Cons other = obj as Cons;
            if (other.car != null && this.car != null) {
                if (other.cdr != null && this.cdr != null) {
                    System.Diagnostics.Debug.WriteLine($"{other.car}.Equals({this.car}) && {other.cdr}.Equals({this.cdr})");
                    return other.car.Equals(this.car) && other.cdr.Equals(this.cdr);
                }
                else {
                    if (other.car.Equals(this.car)) {
                        return other.cdr == null && this.cdr == null;
                    }
                    else {
                        return false;
                    }
                }
            }
            else {
                if (other.car == null && this.car == null) {
                    return other.cdr.Equals(this.cdr);
                }
                else {
                    return false;
                }
            }
        }

        public static bool Equals(Cons cons1, Cons cons2) {
            if (cons1 != null && cons2 != null) {
                return cons1.car.Equals(cons2.car) && cons1.cdr.Equals(cons2.cdr);
            }
            else {
                return cons1 == null && cons2 == null;
            }
        }

        public override int GetHashCode() {
            unchecked {
                return 17 * 23 + (car.GetHashCode() ^ cdr.GetHashCode());
            }
        }
    }

}