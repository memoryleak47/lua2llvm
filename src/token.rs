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

fn alpha(chr: char) -> bool {
   ('A'..='Z').contains(&chr) || ('a'..='z').contains(&chr) || chr == '_'
}

fn numeric(chr: char) -> Option<u32> { // returns the digit
   if ('0'..='9').contains(&chr) {
        Some(chr as u32 - '0' as u32)
   } else { None }
}

// the chars valid in the middle of an ident
fn inident(chr: char) -> bool {
    alpha(chr) || numeric(chr).is_some() || chr == '_'
}

fn whitespace(chr: char) -> bool {
    chr == ' ' || chr == '\n' || chr == '\t'
}

pub fn tokenize(code: &str) -> Vec<Token> {
    let mut tokens = Vec::new();

    let chars: Vec<_> = code.chars().collect();

    let mut i: usize = 0;
    while i < chars.len() {
        let chr = chars[i];

        // multi-line comments
        if matches!(chars[i..], ['-', '-', '[', '[', ..]) {
            if let Some(j) = (0..chars[i..].len()).position(|j| matches!(chars[i..][j..], [']', ']', ..])) {
                i += j + 2;
                continue;
            } else { panic!("unclosed multi-line comment") }
        }

        // single-line comments
        if matches!(chars[i..], ['-', '-', ..]) {
            if let Some(j) = chars[i..].iter().position(|&x| x == '\n') {
                i += j + 1;
                continue;
            } else { break; }
        }

        // multi-char special tokens
        {
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
                i += 2;
                continue;
            }
        }

        // single-char special tokens
        {
            let opttok = match chr {
                '(' => Some(Token::LParen),
                ')' => Some(Token::RParen),
                '[' => Some(Token::LBracket),
                ']' => Some(Token::RBracket),
                '{' => Some(Token::LBrace),
                '}' => Some(Token::RBrace),
                '.' => Some(Token::Dot),
                ':' => Some(Token::Colon),
                ';' => Some(Token::Semicolon),
                '=' => Some(Token::Equals),
                ',' => Some(Token::Comma),
                '+' => Some(Token::Plus),
                '-' => Some(Token::Minus),
                '*' => Some(Token::Mul),
                '/' => Some(Token::Div),
                '%' => Some(Token::Mod),
                '<' => Some(Token::Lt),
                '>' => Some(Token::Gt),
                '^' => Some(Token::Pow),
                '#' => Some(Token::Len),
                _ => None,
            };

            if let Some(tok) = opttok {
                tokens.push(tok);
                i += 1;
                continue;
            }
        }

        // strings
        if let '"' | '\'' = chr {
            let terminator = chr;
            let mut s = String::new();
            i += 1;
            loop {
                match chars[i] {
                    '\\' => {
                        i += 1;
                        match chars[i] {
                            '\\' => s.push('\\'),
                            'n' => s.push('\n'),
                            't' => s.push('\t'),
                            'r' => s.push('\r'),
                            _ => panic!("invalid \\<char> sequence!"),
                        }
                    }
                    c if c == terminator => {
                        tokens.push(Token::LiteralStr(s));
                        i += 1;
                        break;
                    },
                    c => {
                        s.push(c);
                        i += 1;
                    },
                }
            }
            continue;
        }

        // idents and keywords
        if alpha(chr) {
            let mut s = String::from(chr);
            i += 1;
            while inident(chars[i]) {
                s.push(chars[i]);
                i += 1;
            }

            tokens.push(match &*s {
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
                "local" => Token::Local,
                _ => Token::Ident(s),
            });

            continue;
        }

        // numbers
        if let Some(digit) = numeric(chr) {
            let mut num = digit as f64;
            i += 1;

            while let Some(d) = numeric(chars[i]) {
                num = num * 10.0 + d as f64;
                i += 1;
            }
            if chars[i] == '.' {
                i += 1;
                let mut fac = 1.0;
                while let Some(d) = numeric(chars[i]) {
                    fac /= 10.0;
                    num += fac * d as f64;
                    i += 1;
                }
            }

            tokens.push(Token::LiteralNum(num));
            continue;
        }

        if !whitespace(chr) { panic!("cannot tokenize!") }

        i += 1;
    }

    tokens
}
