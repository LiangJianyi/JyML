using System.Collections.Generic;
using System.Linq;

namespace JymlParser {
    public class Tokenizer {
        private string _text;
        private char[] _seperators;
        private char[] _singles;

        public string[] Tokens { get; private set; }

        /// <summary>
        /// 实例化一个 Splitor
        /// </summary>
        /// <param name="text">源字符串</param>
        /// <param name="seperators">依据该数组中的字符进行切割</param>
        /// <param name="singles">需要把单个字符作为 token 的字符集</param>
        public Tokenizer(string text, char[] seperators, char[] singles) {
            this._text = text;
            this._seperators = seperators;
            this._singles = singles;
            this.Tokens = MakeTokens();
        }

        /// <summary>
        /// 切割字串
        /// </summary>
        /// <returns></returns>
        private string[] MakeTokens() {
            string[] temp = null;
            for (int i = 0; i < _text.Length; i++) {
                if (_singles.Contains(_text[i])) {
                    FillArr(ref temp, _text.Substring(i, 1));
                }
                else if (_seperators.Contains(_text[i])) {
                    continue;
                }
                else {
                    (int start, int end) = GetRange(ref i);
                    FillArr(ref temp, _text.Substring(start, end - start + 1));
                }
            }
            return temp;
        }

        /// <summary>
        /// 根据传入的索引获取组成 token 的字符范围
        /// </summary>
        /// <param name="index">代表 token 首字符所在的位置</param>
        /// <returns></returns>
        private (int Start, int End) GetRange(ref int index) {
            int start = index;
            while ((index < this._text.Length) &&
                    (!_seperators.Contains(_text[index])) &&
                    (!_singles.Contains(_text[index]))) {
                index++;
            }
            index -= 1;
            int end = index;
            return (start, end);
        }

        /// <summary>
        /// 把字符填充到保存 token 的数组
        /// </summary>
        /// <param name="source">保存 token 的数组的引用</param>
        /// <param name="c">进行填充的字符</param>
        private void FillArr(ref string[] source, string c) {
            if (source == null || source.Length == 0) {
                source = new string[] { c };
            }
            else {
                string[] arr = new string[source.Length + 1];
                for (int i = 0; i < source.Length; i++) {
                    arr[i] = source[i];
                }
                arr[arr.Length - 1] = c;
                source = arr;
            }
        }

        /// <summary>
        /// 清除 token 两边的控制字符和空白字符以及无效的 token
        /// </summary>
        /// <param name="tokens"></param>
        public void CleanUpTokens() {
            List<int> deleteTokenIndex = new List<int>();
            bool f(ref string str) {
                bool hadSymbol = false;
                int symbolStartIndex = -1;
                for (int i = 0; i < str.Length; i++) {
                    if (char.IsControl(str[i]) && char.IsWhiteSpace(str[i])) {
                        if (hadSymbol) {
                            str = str.Substring(symbolStartIndex, i);
                        }
                        else {
                            str = str.Substring(i + 1, str.Length - 1);
                            i = -1;
                        }
                    }
                    else {
                        if (hadSymbol == false) {
                            symbolStartIndex = i;
                            hadSymbol = true;
                        }
                    }
                }
                return hadSymbol;
            }
            for (int i = 0; i < Tokens.Length; i++) {
                if (f(ref Tokens[i]) == false) {
                    deleteTokenIndex.Add(i);
                }
            }
            if (deleteTokenIndex.Count > 0) {
                string[] newtok = new string[Tokens.Length - deleteTokenIndex.Count];
                for (int i = 0, j = 0; j < Tokens.Length; j++) {
                    if (deleteTokenIndex.Contains(j)) {
                        continue;
                    }
                    else {
                        newtok[i] = Tokens[j];
                        i++;
                    }
                }
                Tokens = newtok;
            }
        }
    }
}
