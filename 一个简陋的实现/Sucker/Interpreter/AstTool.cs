using System;
using System.Collections.Generic;
using System.Linq;
using JymlTypeSystem;

namespace JymlAST {
    static class AstTool {
        public static T[] ToArray<T>(this Cons cons) where T : class {
            T[] arr = new T[cons.Length];
            int i = 0;
            foreach (var item in cons) {
                arr[i++] = item as T;
            }
            return arr;
        }
    }
}
