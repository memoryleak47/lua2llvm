#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    LParen, RParen, LBracket, RBracket, LBrace, RBrace, Comma, Equals, Dot, Colon, Semicolon,

    // keywords
    Function, End, Return,

    // ops
    Plus, Minus, Mul, Div, Mod,
    Or, And,
    Lt, Le, Gt, Ge,
    IsEqual, IsNotEqual,
    Concat, Pow,

    Not, Hash,

    Ident(String),
    LiteralNum(u32),
}

enum TokenState {
    Start,
    InIdent(String),
    InNum(u32),
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
                    _ => Token::Ident(ident.clone()),
                });
                state = Start;
            }
        }

        if let InNum(num) = &mut state {
            if let Some(digit) = numeric(chr) {
                *num = 10 * *num + digit;
                i += 1; continue;
            } else {
                tokens.push(Token::LiteralNum(*num));
                state = Start;
            }
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
            '#' => tokens.push(Token::Hash),
            c if alpha(c) => {
                state = InIdent(String::from(c));
            },
            c if numeric(c).is_some() => {
                state = InNum(numeric(c).unwrap());
            },
            c if whitespace(c) => {},
            _ => panic!("cannot tokenize!"),
        }

        i += 1;
    }

    tokens
}
