let core_uart_addr = 'h_6230_0000;
Recipe r = rSeq ( rBlock (
    recipeDelay (2000)
  , debugUnitSendReset (verbosity, False)
  , fake16550TransmitData (verbosity, 'hdeadbeef)
  , debugUnitSendMemRead (verbosity, core_uart_addr)
  , debugUnitSendMemWrite (verbosity, core_uart_addr, 'hb00bf00d)
  , fake16550ReceiveData (verbosity)
  , done.send
  ));
