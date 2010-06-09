# Overview
logging is an erlang library, which provides the log facility. it is inspired by python logging lib.
using loggging, you can control your log output style(Handler), define log entry format(Formater),
set the log filter.

# Usage
## simple usage
    logging:start_sup(),
    logging:start_logger(),
    logging:debug("this is debug log"),
    logging:info("this is information log"),
    logging:warning("this is warning log"),
    logging:error("this is an error record"),
    logging:critical("this is an critical error message")

## set the logger name
    logging:start_sup(),
    logging:start_logger("access"),
    logging:debug("access", "this is debug log"),
    logging:info("access", "this is information log"),
    logging:warning("access", "this is warning log"),
    logging:error("access", "this is an error record"),
    logging:critical("access", "this is an critical error message")
 
you can define an macro to simplify your type:
   -define(ACCESS_LOGGER(D), logging:info("access", D)).
   -define(ACCESS_LOGGER(F, D), logging:info("access", io_lib:format(F, D))).

## set the handler, formatter and filter 
    logging:start_sup(),
    Handler = tty,
    Formatter = "~(name)s ~(levelno)s ~(message)s",
    Filter = 
    fun(_Record) ->
        true
    end,
    logging:start_logger("custom", Handler, Formatter, Filter),
    logging:debug("custom", "this is debug log"),
    logging:info("custom", "this is information log"),
    logging:warning("custom", "this is warning log"),
    logging:error("custom", "this is an error record"),
    logging:critical("custom", "this is an critical error message")

