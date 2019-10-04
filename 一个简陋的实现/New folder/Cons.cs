using System.Collections;
using System.Collections.Generic;

namespace JymlParser {
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
    }

}