using System;
using System.Collections.Generic;
using System.Numerics;
using JymlAST;

namespace Interpreter {
    public static class SuckerMLInterpreter {
        public class DayNode : IComparable<DayNode> {
            public int Day { get; private set; }
            public BigInteger Total { get; set; }
            public DayNode(int day, BigInteger total) {
                if (day > 0) {
                    Day = day;
                }
                else {
                    throw new Exception();
                }
                if (total > 0) {
                    Total = total;
                }
                else {
                    throw new Exception($"Total 值必须大于 0。total = {total}");
                }
            }

            int IComparable<DayNode>.CompareTo(DayNode other) {
                if (this.Day == other.Day) {
                    return 0;
                }
                else if (this.Day > other.Day) {
                    return 1;
                }
                else {
                    return -1;
                }
            }
        }
        public class MonthNode : IComparable<MonthNode> {
            public int Month { get; private set; }
            public SortedList<int, DayNode> Days { get; private set; }
            public MonthNode(int month) {
                if (month <= 12 && month > 0) {
                    Month = month;
                }
                else {
                    throw new Exception($"Month 值必须介于 1 到 12 之间。Month = {month}");
                }
                Days = new SortedList<int, DayNode>();
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
                Days = new SortedList<int, DayNode>();
            }

            int IComparable<MonthNode>.CompareTo(MonthNode other) {
                if (this.Month == other.Month) {
                    return 0;
                }
                else if (this.Month > other.Month) {
                    return 1;
                }
                else {
                    return -1;
                }
            }
        }
        public class YearNode : IComparable<YearNode> {
            public BigInteger Year { get; private set; }
            public SortedList<int, MonthNode> Months { get; private set; }
            public YearNode(BigInteger year) {
                if (year > 0) {
                    Year = year;
                }
                else {
                    throw new Exception($"无效的年份表示。year：{year}");
                }
                Months = new SortedList<int, MonthNode>();
            }

            int IComparable<YearNode>.CompareTo(YearNode other) {
                if (this.Year == other.Year) {
                    return 0;
                }
                else if (this.Year > other.Year) {
                    return 1;
                }
                else {
                    return -1;
                }
            }
        }

        private static void Add(this SortedList<int, DayNode> days, DayNode day) {
            try {
                days.Add(day.Day, day);
            }
            catch (ArgumentException) {
                days[day.Day].Total += day.Total;
            }
        }

        private static void Add(this SortedList<int, MonthNode> months, MonthNode month) {
            try {
                months.Add(month.Month, month);
            }
            catch (ArgumentException) {
                months[month.Month].Days.Addpend(month.Days);
            }
        }

        private static void Add(this SortedList<BigInteger, YearNode> years, YearNode year) {
            try {
                years.Add(year.Year, year);
            }
            catch (ArgumentException) {
                years[year.Year].Months.Addpend(year.Months);
            }
        }

        private static void Addpend(this SortedList<int, DayNode> days, SortedList<int, DayNode> newDays) {
            foreach (var item in newDays) {
                days.Add(item.Value);
            }
        }

        private static void Addpend(this SortedList<int, MonthNode> months, SortedList<int, MonthNode> newMonths) {
            foreach (var item in newMonths) {
                months.Add(item.Value);
            }
        }

        private static DayNode EvalDay(Cons cons) {
            if ((cons.car as string).ToLower() == "day-total") {
                int day = Convert.ToInt32((cons.cdr as Cons).car as string);
                BigInteger total = BigInteger.Parse(((cons.cdr as Cons).cdr as Cons).car as string);
                return new DayNode(day, total);
            }
            else {
                throw new Exception($"无效的标记：{cons.car}");
            }
        }

        private static MonthNode EvalMonth(Cons cons) {
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

        private static YearNode EvalYear(Cons cons) {
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

        public static SortedList<BigInteger, YearNode> Eval(Cons ast) {
            SortedList<BigInteger, YearNode> years = new SortedList<BigInteger, YearNode>();
            foreach (Cons node in ast) {
                years.Add(EvalYear(node));
            }
            return years;
        }
    }
}
