using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Numerics;
using JymlAST;

namespace Interpreter {
    static class SuckerMLInterpreter {
        public class DayNode {
            public BigInteger Day { get; private set; }
            public BigInteger Total { get; private set; }
            public DayNode(BigInteger day, BigInteger total) {
                Day = day;
                Total = total;
            }
        }
        public class MonthNode {
            public BigInteger Month { get; private set; }
            public List<DayNode> Days { get; private set; }
            public MonthNode(BigInteger month) {
                Month = month;
                Days = new List<DayNode>();
            }
            public MonthNode(string exp) {
                switch (exp.ToLower()) {
                    case "jan":
                        Month = 1;
                        break;
                    case "feb":
                        Month = 2;
                        break;
                    case "mar":
                        Month = 3;
                        break;
                    case "apr":
                        Month = 4;
                        break;
                    case "may":
                        Month = 5;
                        break;
                    case "jun":
                        Month = 6;
                        break;
                    case "jul":
                        Month = 7;
                        break;
                    case "aug":
                        Month = 8;
                        break;
                    case "sep":
                        Month = 9;
                        break;
                    case "oct":
                        Month = 10;
                        break;
                    case "nov":
                        Month = 11;
                        break;
                    case "dec":
                        Month = 12;
                        break;
                    default:
                        throw new Exception("无效的月份缩写");
                }
                Days = new List<DayNode>();
            }
        }
        public class YearNode {
            public BigInteger Year { get; private set; }
            public List<MonthNode> Months { get; private set; }
            public YearNode(BigInteger year) {
                Year = year;
                Months = new List<MonthNode>();
            }
        }

        public static DayNode EvalDay(Cons cons) {
            if ((cons.car as string).ToLower() == "day-total") {
                BigInteger day = BigInteger.Parse((cons.cdr as Cons).car as string);
                BigInteger total = BigInteger.Parse(((cons.cdr as Cons).cdr as Cons).car as string);
                return new DayNode(day, total);
            }
            else {
                throw new Exception($"无效的标记：{cons.car}");
            }
        }

        public static MonthNode EvalMonth(Cons cons) {
            if ((cons.car as string).ToLower() == "month") {
                MonthNode month = new MonthNode((cons.cdr as Cons).car as string);
                Cons daysCollection = (cons.cdr as Cons).cdr as Cons;
                foreach (Cons day in daysCollection) {
                    month.Days.Add(EvalDay(day));
                }
                return month;
            }
            else {
                throw new Exception($"无效的标记：{cons.car}");
            }
        }

        public static YearNode EvalYear(Cons cons) {
            if ((cons.car as string).ToLower() == "year") {
                YearNode year = new YearNode(BigInteger.Parse((cons.cdr as Cons).car as string));
                Cons monthCollection = (cons.cdr as Cons).cdr as Cons;
                foreach (Cons month in monthCollection) {
                    year.Months.Add(EvalMonth(month));
                }
                return year;
            }
            else {
                throw new Exception($"无效的标记：{cons.car}");
            }
        }

        public static List<YearNode> Eval(Cons ast) {
            List<YearNode> years = new List<YearNode>();
            foreach (Cons node in ast) {
                years.Add(EvalYear(node));
            }
            return years;
        }
    }
}
