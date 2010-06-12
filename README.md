# Overview
logging is an erlang library, which provides the log facility. it is inspired by python logging lib.
using loggging, you can control your log output style(Handler), define log entry format(Formater),
set the log filter.

# Usage
the below is the usage code segments

## simple usage
    logging:start("access"), % start the `access` logger
    Logger = logging:get_logger("access"),
    Logger:debug(?MODULE, ?LINE, "this is debug log"),
    Logger:info(?MODULE, ?LINE, "this is information log"),
    Logger:warning(?MODULE, ?LINE, "this is warning log"),
    Logger:error(?MODULE, ?LINE, "this is an error record"),
    Logger:critical(?MODULE, ?LINE, "this is an critical error message")

you can define an macro to simplify your type:

   -define(ACCESS_LOGGER(D), (logging:get_logger("access")):info(?MODULE, ?LINE, D)).

## set the handler, formatter and filter 
    logging:start_sup(),
    Handler = tty,
    Formatter = "(name) - (levelno) (message)",
    logging:start_link("custom", ?DEBUG, {logging_tty_h, []}, Formatter),
    Logger = logging:get_logger("custom"),
    Logger:add_level("mylevel", 10),

    Logger:debug("this is debug log"),
    Logger:info("this is information log"),
    Logger:warning("this is warning log"),
    Logger:error("this is an error record"),
    Logger:critical("this is an critical error message")
