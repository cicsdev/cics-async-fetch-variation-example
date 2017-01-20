# CICS Asynchronous API Fetch variation Example

This example has a parent program ASYNCPG1 to demonstrate the use of 
EXEC CICS FETCH commands with and without TIMEOUT, NOSUSPEND options.
The application also have four child programs:
ASYNCCH1, ASYNCCH2, ASYNCCH3, ASYNCCH4.
ASYNCCH1 has a delay of 2 seconds.
ASYNCCH2 has a delay of 5 seconds.
ASYNCCH3 has a delay of 6 seconds.
ASYNCCH4 has a delay of 1 second.

ASYNCPG1 starts 4 asynchronous child tasks. It then does 3 differnt 
flavours of fetch to get the response, according to the business 
requirement as following:
- definitely need the response from the first child so
use FETCH CHILD with no SUSPEND or TIMEOUT option.
- can wait for the response from the second child for a certain
period so use FETCH CHILD TIMEOUT
- try to get response from either third or fouth child
but cannot wait so use FETCH ANY NOSUSPEND

## Set Up

1. Download the source code from this GitHub repository as a .zip file and
   extract to your preferred location.
2. Compile the COBOL source code in the _src_ directory.
3. Define the transactions and programs in your CSD, and add the load library to
   the RPL (or define a library resource in the CSD).

Here are some CSD definitions we made earlier:
```
DELETE GROUP(FVECSD)

DEFINE PROGRAM(ASYNCPG1) GROUP(FVECSD) STATUS(ENABLED)
DEFINE TRANSACTION(PG1) GROUP(FVECSD) PROGRAM(ASYNCPG1)

DEFINE PROGRAM(ASYNCCH1) GROUP(FVECSD) STATUS(ENABLED)
DEFINE TRANSACTION(SUB1) GROUP(FVECSD) PROGRAM(ASYNCCH1)

DEFINE PROGRAM(ASYNCCH2) GROUP(FVECSD) STATUS(ENABLED)
DEFINE TRANSACTION(SUB2) GROUP(FVECSD) PROGRAM(ASYNCCH2)

DEFINE PROGRAM(ASYNCCH3) GROUP(FVECSD) STATUS(ENABLED)
DEFINE TRANSACTION(SUB3) GROUP(FVECSD) PROGRAM(ASYNCCH2)

DEFINE PROGRAM(ASYNCCH4) GROUP(FVECSD) STATUS(ENABLED)
DEFINE TRANSACTION(SUB4) GROUP(FVECSD) PROGRAM(ASYNCCH4)
``` 
## Running the Example

### Using a CICS Terminal

At the terminal screen, enter the transaction ID you wish to run.

The screen may seem to hang, but that's just the time it takes for the request
to complete. When the transaction finishes, it will print a line on 
terminal screen to indicate the fetch status from each child:
ASYNCPG1 SUB1(OK) SUB2(NO) SUB3(  ) SUB4(OK)

## License

This project is licensed under [Apache License Version 2.0](LICENSE).
