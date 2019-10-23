using JymlAST;
using JymlTypeSystem;
using System;
using System.Linq;
using System.Collections.Generic;

namespace JymlEnvironment {
    public class JymlEnviroment {
        public JymlEnviroment Enviroment { get; set; }

        public class Restraint {
            public string Variable { get; set; }
            public JymlType Value { get; set; }
            public Restraint(string var, JymlType val) {
                Variable = var;
                Value = val;
            }
        }
        public class Frame {
            public LinkedList<Restraint> Restraints { get; private set; } = new LinkedList<Restraint>();
            public Frame(LinkedList<Restraint> restraints) => Restraints = restraints;
            public void AddBidingToFrame(string var, JymlType val) => this.Restraints.AddFirst(new Restraint(var, val));
            public Restraint this[string i] => (from r in Restraints where r.Variable == i select r).First();
        }

        public Frame FrameNode { get; private set; }

        public Cons ExtendEnvironment(string[] variables, JymlType[] values, JymlEnviroment baseEnv) {
            if (variables.Length == values.Length) {
                LinkedList<Restraint> restraints = new LinkedList<Restraint>();
                for (int i = 0; i < variables.Length; i++) {
                    restraints.AddFirst(new Restraint(variables[i], values[i]));
                }
                return new Cons(new Frame(restraints), baseEnv);
            }
            else {
                throw new Exception("变量名的数量与其值的数量不匹配。");
            }
        }

        public void SetVariableValue(string var, JymlType val) => FrameNode[var].Value = val;

        public void DefineVariable(string var,JymlType val) {
            if (FrameNode[var]!=null) {
                throw new Exception($"变量 {var} 重定义。");
            }
            else {
                FrameNode.AddBidingToFrame(var, val);
            }
        }

        public static Cons SetUpEnvironment() {
            JymlEnviroment initialEnv = new JymlEnviroment();
            return initialEnv.ExtendEnvironment(
                JymlType._primitiveProcedures.Keys.ToArray(),
                JymlType._primitiveProcedures.Values.ToArray(),
                null
            );
        }
    }
}