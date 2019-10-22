using JymlAST;
using JymlTypeSystem;
using System;
using System.Collections.Generic;

namespace Interpreter {
    public class JymlEnviroment {
        public class Restraint {
            public string Variable { get; set; }
            public JymlType Value { get; set; }
            public Restraint(string var, JymlType val) {
                Variable = var;
                Value = val;
            }
        }
        public class Frame {
            private LinkedList<Restraint> Restraints = new LinkedList<Restraint>();
            public Frame(LinkedList<Restraint> restraints) => Restraints = restraints;
            public void AddBidingToFrame(string var, JymlType val) => this.Restraints.AddFirst(new Restraint(var, val));
        }
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
    }
}