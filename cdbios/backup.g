stzip -a cdbioszp.zip cd_sampm.s cd_switc.s cdbios.s pftest.s cd_asamp.s cd_samp.s cd_test.s check.s
stzip -a cdbioszp.zip echo.gas fifo_isr.gas fifo_r3.gas fifomat.gas fiforead.gas match.gas bangp.gas
stzip -a cdbioszp.zip fifo.gas fifo_r1.gas fifo_rea.gas fifomreg.gas fifov1.gas test.gas cd_samp.gas
stzip -a cdbioszp.zip fifo_r2.gas testf.gas echo.das inout.das jeridata.das
stzip -a cdbioszp.zip cd.inc cd_inits.inc cdbios.inc
stzip -a cdbioszp.zip backup.g theway
stzip -c cdbioszp.zip
cp cdbioszp.zip a:\cdbioszp.zip
diff cdbioszp.zip a:\cdbioszp.zip

