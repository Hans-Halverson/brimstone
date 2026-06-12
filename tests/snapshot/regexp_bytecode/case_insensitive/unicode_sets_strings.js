// Case folds to the same string
/[\q{AB|Ab|aB|ab}]/iv;
/[\q{AB}&&\q{ab}]/iv;
/[\q{AB}--\q{ab}]/iv;