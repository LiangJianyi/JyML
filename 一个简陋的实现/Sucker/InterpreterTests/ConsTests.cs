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
        [TestMethod()]
        public void ConsTest() {
            Assert.Fail();
        }

        [TestMethod()]
        public void StringArrayExplicitToConsTest() {
            Cons expected = new Cons("a", new Cons("b", new Cons("c", new Cons("d", new Cons("e", new Cons("f", new Cons("g")))))));
            Cons acutal = ((Cons)new string[] { "a", "b", "c", "d", "e", "f", "g" });
            Assert.AreEqual<Cons>(expected, acutal);
        }

        [TestMethod()]
        public void FromListTest() {
            Assert.Fail();
        }

        [TestMethod()]
        public void FromArrayTest() {
            Assert.Fail();
        }

        [TestMethod()]
        public void GetEnumeratorTest() {
            Assert.Fail();
        }

        [TestMethod()]
        public void ToStringTest() {
            Assert.Fail();
        }

        [TestMethod()]
        public void EqualsTest() {
            Assert.Fail();
        }

        [TestMethod()]
        public void EqualsTest1() {
            Assert.Fail();
        }

        [TestMethod()]
        public void GetHashCodeTest() {
            Assert.Fail();
        }
    }
}