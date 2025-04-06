use brimstone_core::common::terminal::RESET;

/// A cell in a table with associated formatting properties.
pub struct TableCell {
    /// The cell's text value.
    value: String,
    /// Formatting modifiers to apply to this cell.
    modifiers: Option<Vec<&'static str>>,
    /// The alignment of the value within the call, e.g. left or right aligned.
    alignment: CellAlignment,
}

/// Whether a value is left or right aligned in a table cell. Defaults to right alignment.
#[derive(Default)]
pub enum CellAlignment {
    Left,
    #[default]
    Right,
}

impl TableCell {
    pub fn new(
        value: String,
        modifiers: Option<Vec<&'static str>>,
        alignment: CellAlignment,
    ) -> TableCell {
        TableCell { value, modifiers, alignment }
    }

    pub fn simple(value: String) -> TableCell {
        TableCell { value, modifiers: None, alignment: CellAlignment::default() }
    }

    pub fn with_modifiers(value: String, modifiers: Vec<&'static str>) -> TableCell {
        TableCell {
            value,
            modifiers: Some(modifiers),
            alignment: CellAlignment::default(),
        }
    }
}

/// A single row in a table.
pub struct TableRow {
    cells: Vec<TableCell>,
}

impl TableRow {
    pub fn new(cells: Vec<TableCell>) -> TableRow {
        TableRow { cells }
    }
}

/// Utility for formatting an entire table with modifiers into a string.
pub struct TableFormatter {
    rows: Vec<TableRow>,
    max_column_length: Vec<usize>,
    num_columns: usize,
}

impl TableFormatter {
    fn new(rows: Vec<TableRow>) -> TableFormatter {
        // Table must be nonempty
        assert!(!rows.is_empty());

        let num_columns = rows[0].cells.len();
        assert!(num_columns > 0);

        // All rows must have same number of columns
        assert!(rows.iter().all(|row| row.cells.len() == num_columns));

        // Find the max number of characters in each column
        let mut max_column_length = vec![0; num_columns];

        for row in &rows {
            for (column, cell) in row.cells.iter().enumerate() {
                let cell_length = cell.value.len();
                max_column_length[column] = max_column_length[column].max(cell_length);
            }
        }

        TableFormatter { rows, max_column_length, num_columns }
    }

    /// Format a table formed from a list of rows.
    pub fn format(rows: Vec<TableRow>) -> String {
        let formatter = TableFormatter::new(rows);
        formatter.build()
    }

    fn build(&self) -> String {
        let mut lines = vec![];

        lines.push(self.separator());

        for row in &self.rows {
            lines.push(self.row(row));
            lines.push(self.separator());
        }

        lines.join("\n")
    }

    fn separator(&self) -> String {
        let mut row = "+-".to_owned();

        for i in 0..(self.num_columns - 1) {
            row.push_str(&"-".repeat(self.max_column_length[i]));
            row.push_str("-+-");
        }

        row.push_str(&"-".repeat(self.max_column_length[self.num_columns - 1]));
        row.push_str("-+");

        row
    }

    fn cell(&self, cell: &TableCell, column_index: usize) -> String {
        let padding_count = self.max_column_length[column_index] - cell.value.len();
        let padding = " ".repeat(padding_count);

        // If cell has modifiers prepend them then append a reset
        let modified_value = if let Some(modifiers) = &cell.modifiers {
            format!("{}{}{}", modifiers.join(""), cell.value, RESET)
        } else {
            cell.value.clone()
        };

        match cell.alignment {
            CellAlignment::Left => format!("{}{}", modified_value, padding),
            CellAlignment::Right => format!("{}{}", padding, modified_value),
        }
    }

    fn row(&self, table_row: &TableRow) -> String {
        let mut row = "| ".to_owned();

        for i in 0..(self.num_columns - 1) {
            row.push_str(&self.cell(&table_row.cells[i], i));
            row.push_str(" | ");
        }

        row.push_str(&self.cell(table_row.cells.last().unwrap(), self.num_columns - 1));
        row.push_str(" |");

        row
    }
}
