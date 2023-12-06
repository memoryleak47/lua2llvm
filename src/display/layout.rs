use crate::layout::*;
use std::fmt::{self, Formatter, Display};

impl Display for Layout {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (loc, res) in &self.table_layouts {
            write!(f, "layout {} as {};\n", loc, res)?;
        }
        Ok(())
    }
}

impl Display for TableLayout {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TableLayout::HashTable => write!(f, "hashtable"),
            TableLayout::Struct(vals) => {
                write!(f, "struct {{")?;
                for (i, v) in vals.iter().enumerate() {
                    write!(f, "{}", v)?;
                    if i < vals.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "}}")?;
                Ok(())
            }
        }
    }
}

