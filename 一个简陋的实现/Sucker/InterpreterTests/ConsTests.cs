using Microsoft.VisualStudio.TestTools.UnitTesting;
using JymlAST;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace JymlAST.Tests {
    [TestClass()]
    public class ConsTests {
        private readonly Cons _expected = new Cons("a", new Cons("b", new Cons("c", new Cons("d", new Cons("e", new Cons("f", new Cons("g")))))));

        [TestMethod()]
        public void StringArrayExplicitToConsTest() {
            Cons acutal1 = ((Cons)new string[] { "a", "b", "c", "d", "e", "f", "g" });
            Cons acutal2 = ((Cons)new string[] { "a", "b", "c", "d", "e", "f", "G" });
            Cons acutal3 = ((Cons)new string[] { null, "b", "c", "d", "e", "f", "g" });
            Assert.AreEqual<Cons>(_expected, acutal1);
            Assert.AreNotEqual<Cons>(_expected, acutal2);
            Assert.AreNotEqual<Cons>(_expected, acutal3);
        }

        [TestMethod()]
        public void FromListTest() {
            List<string> actual1 = new List<string>() { "a", "b", "c", "d", "e", "f", "g" };
            List<string> actual2=new List<string>() { "a", "b", "c", "d", "e", "f", "G" };
            List<string> actual3 = new List<string>() { "A", "b", "c", "d", "e", "f", "G" };
            Assert.AreEqual<Cons>(_expected, Cons.FromList(actual1));
            Assert.AreNotEqual<Cons>(_expected, Cons.FromList(actual2));
            Assert.AreNotEqual<Cons>(_expected, Cons.FromList(actual3));
        }

        [TestMethod()]
        public void FromArrayTest() {
            string[] actual1 = new string[] { "a", "b", "c", "d", "e", "f", "g" };
            string[] actual2 = new string[] { "a", "b", "c", "d", "e", "f", "G" };
            string[] actual3 = new string[] { "A", "b", "c", "d", "e", "f", "G" };
            Assert.AreEqual<Cons>(_expected, Cons.FromList(actual1));
            Assert.AreNotEqual<Cons>(_expected, Cons.FromList(actual2));
            Assert.AreNotEqual<Cons>(_expected, Cons.FromList(actual3));
        }

        [TestMethod()]
        public void ToStringTest() {
            const string ACTUAL = "(a (b (c (d (e (f (g )))))))";
            Assert.AreEqual<string>(_expected.ToString(), ACTUAL);
        }
    }
}