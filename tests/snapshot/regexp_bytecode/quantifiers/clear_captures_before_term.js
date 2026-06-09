// Inlined min repetitions
/((a)(b)){3}/;

// Loop for min repetitions
/((a)(b)){100}/;

// Inlined max repetitions
/((a)(b)){1,2}/;

// Loop for max repetitions
/((a)(b)){1,100}/;

// Uncapped max repetitions
/((a)(b)){1,}/;

// Cannot be repeated multiple times, no need to clear
/((a)(b)){0,1}/;
/((a)(b)){1,1}/;
/((a)(b)){0,0}/;