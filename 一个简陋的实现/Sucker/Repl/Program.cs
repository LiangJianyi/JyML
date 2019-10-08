using System;
using System.Collections.Generic;
using System.Linq;
using JymlParser;
using JymlAST;
using Interpreter;

namespace Repl {
    class Program {
        static void Main(string[] args) {
            string path = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) + "\\Test Data\\suckerML.mast";
            string text = System.IO.File.ReadAllText(path);
            Cons ast = Parser.GenerateAst(text);
            Test2(ast);

            Console.ReadKey();
        }

        private static void PrintAst(Cons ast) {
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
        }

        private static void Test1() {
            string text = "xxoo\n\t\r " +
                                "\txxoo " +
                                " \n " +
                                " \t\n" +
                                " \ty\n" +
                                "\r\n\txxoo\n " +
                                " \tyohoho!yohohohohhohoohho\n";
            Tokenizer tokenizer = new Tokenizer(text, new char[] { ' ', '\n' }, new char[] { '(', ')', '[', ']' });
            foreach (var item in tokenizer.Tokens) {
                Console.WriteLine(item);
            }
            Console.WriteLine("-----------");
            tokenizer.CleanUpTokens();
            foreach (var item in tokenizer.Tokens) {
                Console.WriteLine(item);
            }
        }

        private static void Test2(Cons ast) {
            SortedList<System.Numerics.BigInteger, SuckerMLInterpreter.YearNode> yearNodes = SuckerMLInterpreter.Eval(ast);
            foreach (var year in yearNodes) {
                Console.WriteLine($"Year {year.Value.Year}:");
                foreach (var month in year.Value.Months) {
                    Console.WriteLine($"    Month {month.Value.Month}:");
                    foreach (var day in month.Value.Days) {
                        Console.WriteLine($"        Day:{day.Value.Day}, Total:{day.Value.Total}");
                    }
                }
            }
        }
    }
}
