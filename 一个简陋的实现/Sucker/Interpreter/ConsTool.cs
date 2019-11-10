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

        public static JymlType[] ParametersToArguments(this Cons cons, Jyml.Environment.JymlEnvironment env) {
            JymlType[] arr = new JymlType[cons.Length];
            int i = 0;
            foreach (var item in cons) {
                try {
                    if (item is string str) {
                        arr[i] = JymlType.CreateType(str); 
                    }
                    else if (item is JymlType jymlType) {
                        arr[i] = jymlType;
                    }
                    else {
                        arr[i] = JymlType.CreateType(item);
                    }
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
