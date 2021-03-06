﻿using System;
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
            string suckerScriptPath = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) + "\\Test Data\\SuckerScript5.mast";
            string suckerScriptText = System.IO.File.ReadAllText(suckerScriptPath);
            string suckerMLPath = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) + "\\Test Data\\suckerML.mast";
            string suckerMLText = System.IO.File.ReadAllText(suckerMLPath);
            PrintTopLevel(Parser.GenerateAst(suckerScriptText).ast);
            TestSuckerMLInterpreter(Parser.GenerateAst(suckerMLText));
            TestEval(Parser.GenerateAst(suckerScriptText));
            //PressureTestForEval(suckerScriptText);
            Console.ReadKey();
        }

        private static void PressureTestForEval(string suckerScriptText) {
            const int COUNT = 1000;
            Console.WriteLine($"Execute TestEval {COUNT} times:");
            System.DateTime startime = System.DateTime.Now;
            for (int i = 0; i < COUNT; i++) {
                Console.WriteLine($"Step {i + 1}：");
                TestEval(Parser.GenerateAst(suckerScriptText));
            }
            Console.WriteLine($"Time consuming：{(System.DateTime.Now - startime).Seconds} s.");
        }

        private static void PrintAst(JymlAST.Cons ast) {
            Console.WriteLine("Print ast...");
            foreach (JymlAST.Cons year in ast) {
                foreach (object item in year) {
                    if (item is string str) {
                        Console.WriteLine(str);
                    }
                    else if (item is JymlAST.Cons cons) {
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

        private static void PrintTopLevel(JymlAST.Cons ast) {
            Console.WriteLine("Print top level...");
            foreach (var item in ast) {
                Console.WriteLine(item);
            }
            Console.WriteLine("\n\n");
        }

        private static void TestSuckerMLInterpreter((Language lang, JymlAST.Cons ast) dispatch) {
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

        private static void TestEval((Language lang, JymlAST.Cons ast) dispatch) {
            Console.WriteLine("Test Eval...");
            JymlAST.Cons res = SuckerInterpreter.Eval(dispatch) as JymlAST.Cons;
            foreach (var item in res) {
                if (item != null) {
                    if (item is JymlAST.Cons cons) {
                        Console.WriteLine(cons.car);
                    }
                    else {
                        Console.WriteLine(item);
                    }
                }
            }
            Console.WriteLine("\n\n");
        }
    }
}
