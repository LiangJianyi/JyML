using System;
using JymlTypeSystem;

namespace JymlAST {
    static class ConsTool {
        public static T[] ToArray<T>(this Cons cons) where T : class {
            T[] arr = new T[cons.Length];
            int i = 0;
            foreach (var item in cons) {
                arr[i++] = item as T;
            }
            return arr;
        }

        public static JymlType[] ConsToArguments(this Cons cons, Jyml.Environment.JymlEnvironment env) {
            JymlType[] arr = new JymlType[cons.Length];
            int i = 0;
            foreach (var item in cons) {
                try {
                    arr[i] = JymlType.CreateType(item as string);
                }
                catch (InvalidCastException) {
                    arr[i] = env.GetVariableValue(item as string);
                }
                i++;
            }
            return arr;
        }

    }
}
