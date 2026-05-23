// Simple test
/\p{Emoji_Keycap_Sequence}/v;

// Strings are de-duped with other strings in the class, should be identical to above
/[\p{Emoji_Keycap_Sequence}\q{#\uFE0F\u20E3|0\uFE0F\u20E3}]/v;

// Empty set
/[\p{Emoji_Keycap_Sequence}--\p{Emoji_Keycap_Sequence}]/v;