``;
`a`;
`abc`;

`abc${asdf + 2}def`;
`abc${1}de${2}f`;
`${1}`;
`${1}${2}`;

`$ ' "`;

// Newlines
`before

after`;

// Line continuation
`before\
\
after`;

`test \n \\ \' \" \` \t \r \b \v \f \0 test`;

`\x00 \x01 \x03 \x0F \x10 \x1F \xFF \xaa \xaF`;

`\u0062 \u{63} \u{000063}`;

`a${{}}b`;