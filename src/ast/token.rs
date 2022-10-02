#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    LParen,
    RParen,
    Comma,
    Equals,
    Plus,
    Function,
    End,
    Return,
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

    for chr in code.chars() {
        if let InIdent(ident) = &mut state {
            if alpha(chr) {
                ident.push(chr);
                continue;
            } else {
                tokens.push(match &**ident {
                    "function" => Token::Function,
                    "end" => Token::End,
                    "return" => Token::Return,
                    _ => Token::Ident(ident.clone()),
                });
                state = Start;
            }
        }

        if let InNum(num) = &mut state {
            if let Some(digit) = numeric(chr) {
                *num = 10 * *num + digit;
                continue;
            } else {
                tokens.push(Token::LiteralNum(*num));
                state = Start;
            }
        }

        match chr {
            '(' => tokens.push(Token::LParen),
            ')' => tokens.push(Token::RParen),
            '=' => tokens.push(Token::Equals),
            ',' => tokens.push(Token::Comma),
            '+' => tokens.push(Token::Plus),
            c if alpha(c) => {
                state = InIdent(String::from(c));
            },
            c if numeric(c).is_some() => {
                state = InNum(numeric(c).unwrap());
            },
            c if whitespace(c) => {},
            c => { panic!("unexpected char {}", c); },
        }
    }

    tokens
}
