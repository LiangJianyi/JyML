using System;
using System.Collections.Generic;
using System.Linq;
using Interpreter;
using JymlAST;
using JymlParser;
using JymlTypeSystem;
using Jyml.Environment;

namespace ManualTest {
    class Program {
        static void Main(string[] args) {
            string suckerScriptPath = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) + "\\Test Data\\SuckerScript.mast";
            string suckerScriptText = System.IO.File.ReadAllText(suckerScriptPath);
            string suckerMLPath= Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) + "\\Test Data\\suckerML.mast";
            string suckerMLText= System.IO.File.ReadAllText(suckerMLPath);
            PrintTopLevel(Parser.GenerateAst(suckerScriptText).ast);
            TestSuckerMLInterpreter(Parser.GenerateAst(suckerMLText));
            TestEval(Parser.GenerateAst(suckerScriptText));
            Console.ReadKey();
        }

        private static void PrintAst(Cons ast) {
            Console.WriteLine("Print ast...");
            foreach (Cons year in ast) {
                foreach (object item in year) {
                    if (item is string str) {
                        Console.WriteLine(str);
                    }
                    else if (item is Cons cons) {
                        Console.WriteLine("month list:");
                        foreach (var month in cons) {
                            Console.WriteLine($"  {month}");
                        }
                    }
                }
            }
            Console.WriteLine($"\n字串化表示：{ast}");
            Console.WriteLine("\n\n");
        }

        private static void PrintTopLevel(Cons ast) {
            Console.WriteLine("Print top level...");
            foreach (var item in ast) {
                Console.WriteLine(item);
            }
            Console.WriteLine("\n\n");
        }

        private static void TestSuckerMLInterpreter((Language lang, Cons ast) dispatch) {
            Console.WriteLine("TestSuckerMLInterpreter...");
            SuckerMLInterpreter.Sucker sucker = SuckerInterpreter.Eval(dispatch) as SuckerMLInterpreter.Sucker;
            foreach (var year in sucker.Years) {
                Console.WriteLine($"Year {year.Value.Year}:");
                foreach (var month in year.Value.Months) {
                    Console.WriteLine($"    Month {month.Value.Month}:");
                    foreach (var day in month.Value.Days) {
                        Console.WriteLine($"        Day:{day.Value.Day}, Total:{day.Value.Total}");
                    }
                }
            }
            Console.WriteLine("\n\n");
        }

        private static void TestJymlEnvironment() {
            Console.WriteLine("TestJymlEnvironment");
            JymlEnvironment env = JymlEnvironment.SetUpEnvironment();
            env = env.ExtendEnvironment(
                variables: new string[] { "var1", "var2", "var3" },
                values: new JymlType[] { JymlType.CreateType("83497526294"), JymlType.CreateType("false"), JymlType.CreateType("\"hello, world\"") }
            );
            try {
                env = env.ExtendEnvironment(
                        variables: new string[] { "var1", "var1", "var3" },
                        values: new JymlType[] { JymlType.CreateType("83497526294"), JymlType.CreateType("false"), JymlType.CreateType("\"hello, world\"") }
                    );
            }
            catch (Exception ex) {
                Console.WriteLine(ex.Message);
            }
            foreach (var e in env) {
                Console.WriteLine(e.FrameNode);
            }
            Console.WriteLine("\n\n");
        }

        private static void TestEval((Language lang, Cons ast) dispatch) {
            Console.WriteLine("Test Eval...");
            Cons res = SuckerInterpreter.Eval(dispatch) as Cons;
            foreach (var item in res) {
                Console.WriteLine(item);
            }
        }
    }
}
