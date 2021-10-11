Recipe r = rSeq ( rBlock (
    //recipeDelay (450000)
    recipeDelay (1000)
  , debugUnitWriteReg (verbosity, 7'h10, 'h80000001)
  , debugUnitReadReg (verbosity, 7'h11) // expected value: 382
  , debugUnitWriteReg (verbosity, 7'h17, 'h3207b0)
  , recipeDelay (1000)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2 got: 1002 <--- he 1 is for "busy"
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 400000d3
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h4, 'h4000b0d3)
  , debugUnitWriteReg (verbosity, 7'h5, 'h0)
  , debugUnitWriteReg (verbosity, 7'h17, 'h3307b0)
  , recipeDelay (1000)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2 got: 1002
  , debugUnitReadReg (verbosity, 7'h11) // expected value: 382
  , debugUnitWriteReg (verbosity, 7'h17, 'h3207b0)
  , recipeDelay (1000)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2 got 1002
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 4000b0d3
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  /*
  , debugUnitWriteReg (verbosity, 7'h17, 'h321000)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321001)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321002)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321003)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321004)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321005)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321006)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321007)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321008)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321009)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h32100a)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h32100b)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h32100c)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h32100d)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h32100e)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h32100f)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321010)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321011)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321012)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321013)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321014)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321015)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321016)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321017)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321018)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321019)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h32101a)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h32101b)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h32101c)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h32101d)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h32101e)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h32101f)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  */
  , debugUnitWriteReg (verbosity, 7'h17, 'h3207b1)
  , recipeDelay (1000)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2 got 1002
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 70000000
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h38) // expected value: 20040807
  , debugUnitWriteReg (verbosity, 7'h38, 'h20457000)
  , debugUnitReadReg (verbosity, 7'h38) // expected value: 20050807
  , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
  , debugUnitWriteReg (verbosity, 7'h39, 'hc0000000)
  , debugUnitWriteReg (verbosity, 7'h3c, 'hdeadbeef)
  , debugUnitReadReg (verbosity, 7'h38) // expected value: 20050807
  /*
  , debugUnitWriteReg (verbosity, 7'h17, 'h321000)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321001)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321002)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321003)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321004)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321005)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321006)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321007)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321008)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321009)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h32100a)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h32100b)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h32100c)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h32100d)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h32100e)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h32100f)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321010)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321011)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321012)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321013)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321014)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321015)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321016)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321017)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321018)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h321019)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h32101a)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h32101b)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h32101c)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h32101d)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h32101e)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitWriteReg (verbosity, 7'h17, 'h32101f)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  */
  , debugUnitWriteReg (verbosity, 7'h17, 'h3207b1)
  , recipeDelay (1000)
  , debugUnitReadReg (verbosity, 7'h16) // expected value: 2 got 1002
  , debugUnitReadReg (verbosity, 7'h4) // expected value: 70000000
  , debugUnitReadReg (verbosity, 7'h5) // expected value: 0
  , debugUnitReadReg (verbosity, 7'h38) // expected value: 20050807
  , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
  , debugUnitReadReg (verbosity, 7'h38) // expected value: 2015b807 got: 20158807
  , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
  , debugUnitWriteReg (verbosity, 7'h39, 'hc0000000)
  , debugUnitReadReg (verbosity, 7'h38) // expected value: 2015b807 got: 20158807 <--- the b says align error, only shows in hardware, not in sim...
  , debugUnitReadReg (verbosity, 7'h3c) // expected value: 0
  , done.send
  ));
