#[derive(Debug, PartialEq)]
pub enum Token {
    LParen, RParen, LBracket, RBracket, LBrace, RBrace, Comma, Equals, Dot, Colon, Semicolon,

    // keywords
    Function, End, Return, Break, If, Else, ElseIf, Then, While, Do, Local,

    // literals
    True, False, Nil,

    // ops
    Plus, Minus, Mul, Div, Mod,
    Or, And,
    Lt, Le, Gt, Ge,
    IsEqual, IsNotEqual,
    Concat, Pow,
    Not, Len,

    Ident(String),
    LiteralNum(f64),
    LiteralStr(String),
}

enum NumState {
    PreDot(Vec<u32>), // 20 would be [2, 0]
    PostDot(Vec<u32>, Vec<u32>), // 20.03 would be PostDot([2, 0], [0, 3])
}

enum TokenState {
    Start,
    InIdent(String),
    InNum(NumState),
    InStr(/*delim: */char, String),
}

fn alpha(chr: char) -> bool {
   ('a'..='z').contains(&chr) || chr == '_'
}

fn numeric(chr: char) -> Option<u32> { // returns the digit
   if ('0'..='9').contains(&chr) {
        Some(chr as u32 - '0' as u32)
   } else { None }
}

fn whitespace(chr: char) -> bool {
    chr == ' ' || chr == '\n' || chr == '\t'
}

pub fn tokenize(code: &str) -> Vec<Token> {
    use TokenState::*;

    let mut tokens = Vec::new();
    let mut state = TokenState::Start;

    let chars: Vec<_> = code.chars().collect();

    let mut i: usize = 0;
    while i < chars.len() {
        let chr = chars[i];

        if let InIdent(ident) = &mut state {
            if alpha(chr) {
                ident.push(chr);
                i += 1; continue;
            } else {
                tokens.push(match &**ident {
                    "function" => Token::Function,
                    "end" => Token::End,
                    "return" => Token::Return,
                    "or" => Token::Or,
                    "and" => Token::And,
                    "not" => Token::Not,
                    "true" => Token::True,
                    "false" => Token::False,
                    "nil" => Token::Nil,
                    "break" => Token::Break,
                    "if" => Token::If,
                    "else" => Token::Else,
                    "elseif" => Token::ElseIf,
                    "then" => Token::Then,
                    "while" => Token::While,
                    "do" => Token::Do,
                    "local" => Token::Do,
                    _ => Token::Ident(ident.clone()),
                });
                state = Start;
            }
        }

        // num
        {
            let numfn = |predigits: &[u32], postdigits: &[u32]| -> f64 {
                let mut num: f64 = 0.0;
                let n = predigits.len();
                for i in 0..n {
                    num += 10f64.powf((n - i - 1) as f64) * predigits[i] as f64;
                }

                let n = postdigits.len();
                for i in 0..n {
                    num += (10f64).powf(-((i+1) as f64)) * postdigits[i] as f64;
                }

                num
            };

            if let InNum(NumState::PreDot(digits)) = &mut state {
                if let Some(digit) = numeric(chr) {
                    digits.push(digit);
                    i += 1; continue;
                } else if chr == '.' {
                    state = InNum(NumState::PostDot(digits.clone(), Vec::new()));
                    i += 1; continue;
                } else {
                    let num = numfn(digits, &[]);
                    tokens.push(Token::LiteralNum(num));
                    state = Start;
                }
            }

            if let InNum(NumState::PostDot(predigits, postdigits)) = &mut state {
                if let Some(digit) = numeric(chr) {
                    postdigits.push(digit);
                    i += 1; continue;
                } else {
                    let num = numfn(predigits, postdigits);
                    tokens.push(Token::LiteralNum(num));
                    state = Start;
                }
            }
        }

        if let InStr(d, s) = &mut state {
            let optchr = match chars[i..] {
                ['\\', '\\', ..] => Some('\\'),
                ['\\', 't', ..] => Some('\t'),
                ['\\', 'r', ..] => Some('\r'),
                ['\\', 'n', ..] => Some('\n'),
                ['\\', del, ..] if *d == del => Some(*d),
                _ => None,
            };
            if let Some(c) = optchr {
                s.push(c);
                i += 2; continue;
            }

            if chr == *d {
                tokens.push(Token::LiteralStr(s.clone()));
                state = Start;
                i += 1; continue;
            } else {
                s.push(chr);
                i += 1; continue;
            }
        }

        // multi-line comments
        if matches!(chars[i..], ['-', '-', '[', '[', ..]) {
            if let Some(j) = (0..chars[i..].len()).position(|j| matches!(chars[i..][j..], [']', ']', ..])) {
                i += j + 2; continue;
            } else { break; }
        }

        // single-line comments
        if matches!(chars[i..], ['-', '-', ..]) {
            if let Some(j) = chars[i..].iter().position(|&x| x == '\n') {
                i += j + 1; continue;
            } else { break; }
        }

        let opttok = match chars[i..] {
            ['<', '=', ..] => Some(Token::Le),
            ['>', '=', ..] => Some(Token::Ge),
            ['=', '=', ..] => Some(Token::IsEqual),
            ['~', '=', ..] => Some(Token::IsNotEqual),
            ['.', '.', ..] => Some(Token::Concat),
            _ => None,
        };

        if let Some(tok) = opttok {
            tokens.push(tok);
            i += 2; continue;
        }

        match chr {
            '(' => tokens.push(Token::LParen),
            ')' => tokens.push(Token::RParen),
            '[' => tokens.push(Token::LBracket),
            ']' => tokens.push(Token::RBracket),
            '{' => tokens.push(Token::LBrace),
            '}' => tokens.push(Token::RBrace),
            '.' => tokens.push(Token::Dot),
            ':' => tokens.push(Token::Colon),
            ';' => tokens.push(Token::Semicolon),
            '=' => tokens.push(Token::Equals),
            ',' => tokens.push(Token::Comma),
            '+' => tokens.push(Token::Plus),
            '-' => tokens.push(Token::Minus),
            '*' => tokens.push(Token::Mul),
            '/' => tokens.push(Token::Div),
            '%' => tokens.push(Token::Mod),
            '<' => tokens.push(Token::Lt),
            '>' => tokens.push(Token::Gt),
            '^' => tokens.push(Token::Pow),
            '#' => tokens.push(Token::Len),
            '"' => {
                state = InStr('"', String::new());
            },
            '\'' => {
                state = InStr('\'', String::new());
            },
            c if alpha(c) => {
                state = InIdent(String::from(c));
            },
            c if numeric(c).is_some() => {
                let digit = numeric(c).unwrap();
                state = InNum(NumState::PreDot(vec![digit]));
            },
            c if whitespace(c) => {},
            _ => panic!("cannot tokenize!"),
        }

        i += 1;
    }

    tokens
}
