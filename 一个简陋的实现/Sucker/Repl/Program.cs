using System;
using System.Collections.Generic;
using System.Linq;
using JymlParser;
using JymlAST;

namespace Repl {
    class Program {
        static void Main(string[] args) {
            string path = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) + "\\Test Data\\suckerML2.mast";
            string text = System.IO.File.ReadAllText(path);
            Cons ast = Parser.GenerateAst(text);
            PrintAst(ast);

            //Test1();

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
    }
}
