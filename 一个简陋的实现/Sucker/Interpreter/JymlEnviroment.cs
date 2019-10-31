using JymlAST;
using JymlTypeSystem;
using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections;

namespace Jyml.Environment {
    public class JymlEnvironment : IEnumerable<JymlEnvironment> {
        public class Restraint {
            public string Variable { get; set; }
            public JymlType Value { get; set; }
            public Restraint(string var, JymlType val) {
                Variable = var;
                Value = val;
            }
            public override string ToString() => $"(Variable:{Variable}, Value:{Value})";
        }
        public class Frame {
            public LinkedList<Restraint> Restraints { get; private set; } = new LinkedList<Restraint>();
            internal Frame() { }
            public Frame(LinkedList<Restraint> restraints) => Restraints = restraints;

            public void AddBindingToFrame(string var, JymlType val) {
                if (this[var] == null) {
                    this.Restraints.AddFirst(new Restraint(var, val));
                }
                else {
                    throw new Exception($"变量 {var} 重定义。");
                }
            }

            public Restraint this[string i] {
                get {
                    foreach (var item in Restraints) {
                        if (item.Variable == i) {
                            return item;
                        }
                    }
                    return null;
                }
            }

            public override string ToString() {
                string s = "Frame: \n";
                foreach (var item in Restraints) {
                    s += $"\t{item}\n";
                }
                return s;
            }
        }

        public JymlEnvironment Enviroment { get; private set; }

        public Frame FrameNode { get; private set; }

        public JymlEnvironment(Frame frame, JymlEnvironment baseEnv) {
            FrameNode = frame;
            Enviroment = baseEnv;
        }

        public JymlEnvironment ExtendEnvironment(string[] variables, JymlType[] values) {
            if (variables.Length == values.Length) {
                Frame frame = new Frame();
                for (int i = 0; i < variables.Length; i++) {
                    frame.AddBindingToFrame(variables[i], values[i]);
                }
                return new JymlEnvironment(frame, this);
            }
            else {
                throw new Exception("变量名的数量与其值的数量不匹配。");
            }
        }

        public void SetVariableValue(string var, JymlType val) => FrameNode[var].Value = val;

        public JymlType GetVariableValue(string var) {
            foreach (var env in this) {
                if (env.FrameNode[var]!=null) {
                    return env.FrameNode[var].Value;
                }
            }
            throw new Exception($"变量 {var} 未定义。");
        }

        public void DefineVariable(string var, JymlType val) {
            FrameNode.AddBindingToFrame(var, val);
        }

        public static JymlEnvironment SetUpEnvironment() {
            LinkedList<Restraint> restraints = new LinkedList<Restraint>();
            string[] primitiveProcedureNames = PrimitiveProcedure.PrimitiveProcedures.Keys.ToArray();
            PrimitiveProcedure[] primitiveProcedureValues = PrimitiveProcedure.PrimitiveProcedures.Values.ToArray();
            for (int i = 0; i < primitiveProcedureNames.Length; i++) {
                restraints.AddFirst(new Restraint(primitiveProcedureNames[i], primitiveProcedureValues[i]));
            }
            JymlEnvironment initialEnv = new JymlEnvironment(new Frame(restraints), null);
            return initialEnv;
        }

        IEnumerator<JymlEnvironment> IEnumerable<JymlEnvironment>.GetEnumerator() {
            JymlEnvironment env = this;
            while (env != null) {
                yield return env;
                env = env.Enviroment;
            }
            yield break;
        }

        IEnumerator IEnumerable.GetEnumerator() {
            JymlEnvironment env = this;
            while (env != null) {
                yield return env;
                env = env.Enviroment;
            }
            yield break;
        }
    }
}