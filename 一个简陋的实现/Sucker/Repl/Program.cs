using System;
using System.Collections.Generic;
using System.Linq;
using JymlParser;
using JymlAST;

namespace Repl {
    class Program {
        static void Main(string[] args) {
            string path=Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) + "\\Test Data\\suckerML.mast";
            string text = System.IO.File.ReadAllText(path);
            Cons ast = Parser.GenerateAst(text);
            Console.WriteLine(ast);

            Console.ReadKey();
        }
    }
}
