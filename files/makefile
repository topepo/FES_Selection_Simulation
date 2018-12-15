SHELL = /bin/bash
 R    ?= R 
 RCMD  =@$(R) CMD
 TIMESTAMP = $(shell  date '+%Y-%m-%d-%H-%M')
 here=${PWD}/..
 all:  effects_0500_100_0013.RData effects_1000_050_0013.RData effects_1000_000_0013.RData \
	 effects_1000_100_0013.RData effects_1000_025_0013.RData effects_0500_050_0013.RData \
	 effects_0500_200_0013.RData effects_1000_200_0013.RData effects_0500_000_0013.RData \
	 effects_1000_150_0013.RData effects_0500_150_0013.RData effects_0500_025_0013.RData \
	 effects_0500_100_0156.RData effects_1000_150_0156.RData effects_0500_025_0156.RData \
	 effects_0500_050_0156.RData effects_1000_000_0156.RData effects_0500_200_0156.RData \
	 effects_1000_025_0156.RData effects_0500_000_0156.RData effects_1000_100_0156.RData \
	 effects_1000_050_0156.RData effects_0500_150_0156.RData effects_1000_200_0156.RData \
	 effects_1000_150_0523.RData effects_0500_150_0523.RData effects_0500_000_0523.RData \
	 effects_1000_100_0523.RData effects_1000_025_0523.RData effects_0500_025_0523.RData \
	 effects_0500_100_0523.RData effects_1000_000_0523.RData effects_0500_200_0523.RData \
	 effects_0500_050_0523.RData effects_1000_200_0523.RData effects_1000_050_0523.RData \
	 effects_1000_025_0666.RData effects_1000_000_0666.RData effects_1000_050_0666.RData \
	 effects_1000_100_0666.RData effects_0500_050_0666.RData effects_0500_025_0666.RData \
	 effects_0500_100_0666.RData effects_0500_150_0666.RData effects_0500_200_0666.RData \
	 effects_1000_150_0666.RData effects_1000_200_0666.RData effects_0500_000_0666.RData \
	 effects_0500_200_0804.RData effects_1000_025_0804.RData effects_0500_100_0804.RData \
	 effects_0500_150_0804.RData effects_1000_200_0804.RData effects_1000_100_0804.RData \
	 effects_1000_150_0804.RData effects_1000_000_0804.RData effects_0500_050_0804.RData \
	 effects_0500_000_0804.RData effects_0500_025_0804.RData effects_1000_050_0804.RData \
	 effects_1000_100_0871.RData effects_0500_000_0871.RData effects_0500_050_0871.RData \
	 effects_0500_100_0871.RData effects_1000_200_0871.RData effects_1000_050_0871.RData \
	 effects_0500_025_0871.RData effects_1000_150_0871.RData effects_1000_000_0871.RData \
	 effects_1000_025_0871.RData effects_0500_150_0871.RData effects_0500_200_0871.RData \
	 effects_0500_025_1030.RData effects_0500_100_1030.RData effects_1000_100_1030.RData \
	 effects_0500_000_1030.RData effects_0500_200_1030.RData effects_1000_050_1030.RData \
	 effects_1000_150_1030.RData effects_0500_050_1030.RData effects_1000_000_1030.RData \
	 effects_1000_025_1030.RData effects_1000_200_1030.RData effects_0500_150_1030.RData \
	 effects_1000_025_1376.RData effects_1000_050_1376.RData effects_1000_000_1376.RData \
	 effects_0500_050_1376.RData effects_1000_150_1376.RData effects_0500_000_1376.RData \
	 effects_0500_100_1376.RData effects_1000_200_1376.RData effects_0500_025_1376.RData \
	 effects_0500_150_1376.RData effects_1000_100_1376.RData effects_0500_200_1376.RData \
	 effects_0500_100_1571.RData effects_1000_025_1571.RData effects_1000_150_1571.RData \
	 effects_1000_050_1571.RData effects_0500_200_1571.RData effects_0500_000_1571.RData \
	 effects_0500_150_1571.RData effects_0500_050_1571.RData effects_0500_025_1571.RData \
	 effects_1000_200_1571.RData effects_1000_000_1571.RData effects_1000_100_1571.RData \
	 effects_1000_025_1715.RData effects_1000_150_1715.RData effects_1000_000_1715.RData \
	 effects_0500_000_1715.RData effects_0500_100_1715.RData effects_1000_100_1715.RData \
	 effects_0500_025_1715.RData effects_1000_200_1715.RData effects_1000_050_1715.RData \
	 effects_0500_050_1715.RData effects_0500_200_1715.RData effects_0500_150_1715.RData \
	 effects_0500_150_1925.RData effects_1000_050_1925.RData effects_0500_200_1925.RData \
	 effects_0500_100_1925.RData effects_1000_200_1925.RData effects_0500_000_1925.RData \
	 effects_1000_100_1925.RData effects_1000_150_1925.RData effects_1000_025_1925.RData \
	 effects_0500_050_1925.RData effects_1000_000_1925.RData effects_0500_025_1925.RData \
	 effects_1000_025_2110.RData effects_1000_000_2110.RData effects_0500_025_2110.RData \
	 effects_0500_150_2110.RData effects_1000_200_2110.RData effects_0500_200_2110.RData \
	 effects_1000_050_2110.RData effects_0500_050_2110.RData effects_1000_150_2110.RData \
	 effects_1000_100_2110.RData effects_0500_100_2110.RData effects_0500_000_2110.RData \
	 effects_1000_200_2222.RData effects_1000_025_2222.RData effects_1000_050_2222.RData \
	 effects_0500_025_2222.RData effects_1000_000_2222.RData effects_0500_200_2222.RData \
	 effects_1000_100_2222.RData effects_0500_150_2222.RData effects_1000_150_2222.RData \
	 effects_0500_050_2222.RData effects_0500_100_2222.RData effects_0500_000_2222.RData \
	 effects_0500_050_2224.RData effects_1000_000_2224.RData effects_0500_150_2224.RData \
	 effects_0500_100_2224.RData effects_1000_025_2224.RData effects_0500_200_2224.RData \
	 effects_1000_100_2224.RData effects_1000_200_2224.RData effects_1000_050_2224.RData \
	 effects_0500_025_2224.RData effects_0500_000_2224.RData effects_1000_150_2224.RData \
	 effects_0500_200_3297.RData effects_1000_050_3297.RData effects_0500_050_3297.RData \
	 effects_0500_150_3297.RData effects_1000_025_3297.RData effects_1000_200_3297.RData \
	 effects_1000_100_3297.RData effects_1000_150_3297.RData effects_0500_025_3297.RData \
	 effects_1000_000_3297.RData effects_0500_100_3297.RData effects_0500_000_3297.RData \
	 effects_0500_025_3471.RData effects_1000_150_3471.RData effects_0500_050_3471.RData \
	 effects_1000_000_3471.RData effects_1000_100_3471.RData effects_1000_050_3471.RData \
	 effects_0500_150_3471.RData effects_0500_200_3471.RData effects_1000_200_3471.RData \
	 effects_0500_000_3471.RData effects_0500_100_3471.RData effects_1000_025_3471.RData \
	 effects_0500_025_3621.RData effects_0500_200_3621.RData effects_0500_150_3621.RData \
	 effects_1000_150_3621.RData effects_1000_025_3621.RData effects_0500_000_3621.RData \
	 effects_0500_100_3621.RData effects_1000_200_3621.RData effects_1000_100_3621.RData \
	 effects_0500_050_3621.RData effects_1000_000_3621.RData effects_1000_050_3621.RData \
	 effects_0500_050_3666.RData effects_0500_025_3666.RData effects_1000_025_3666.RData \
	 effects_0500_200_3666.RData effects_1000_150_3666.RData effects_1000_100_3666.RData \
	 effects_1000_000_3666.RData effects_0500_150_3666.RData effects_1000_200_3666.RData \
	 effects_0500_100_3666.RData effects_1000_050_3666.RData effects_0500_000_3666.RData \
	 effects_0500_150_4227.RData effects_0500_000_4227.RData effects_1000_025_4227.RData \
	 effects_0500_200_4227.RData effects_1000_000_4227.RData effects_1000_050_4227.RData \
	 effects_1000_150_4227.RData effects_1000_200_4227.RData effects_0500_100_4227.RData \
	 effects_0500_025_4227.RData effects_1000_100_4227.RData effects_0500_050_4227.RData \
	 effects_1000_050_4378.RData effects_0500_050_4378.RData effects_0500_150_4378.RData \
	 effects_1000_025_4378.RData effects_1000_000_4378.RData effects_0500_000_4378.RData \
	 effects_0500_100_4378.RData effects_1000_150_4378.RData effects_0500_025_4378.RData \
	 effects_0500_200_4378.RData effects_1000_200_4378.RData effects_1000_100_4378.RData \
	 effects_1000_150_4399.RData effects_1000_100_4399.RData effects_0500_100_4399.RData \
	 effects_0500_050_4399.RData effects_0500_000_4399.RData effects_1000_050_4399.RData \
	 effects_0500_200_4399.RData effects_0500_150_4399.RData effects_1000_025_4399.RData \
	 effects_1000_000_4399.RData effects_0500_025_4399.RData effects_1000_200_4399.RData \
	 effects_0500_100_4472.RData effects_0500_150_4472.RData effects_1000_000_4472.RData \
	 effects_1000_025_4472.RData effects_1000_050_4472.RData effects_1000_150_4472.RData \
	 effects_0500_025_4472.RData effects_0500_050_4472.RData effects_0500_200_4472.RData \
	 effects_1000_200_4472.RData effects_0500_000_4472.RData effects_1000_100_4472.RData \
	 effects_1000_200_4824.RData effects_1000_000_4824.RData effects_0500_000_4824.RData \
	 effects_1000_050_4824.RData effects_1000_025_4824.RData effects_0500_150_4824.RData \
	 effects_1000_150_4824.RData effects_1000_100_4824.RData effects_0500_050_4824.RData \
	 effects_0500_200_4824.RData effects_0500_025_4824.RData effects_0500_100_4824.RData \
	 effects_1000_050_4911.RData effects_0500_000_4911.RData effects_1000_100_4911.RData \
	 effects_0500_200_4911.RData effects_1000_150_4911.RData effects_1000_025_4911.RData \
	 effects_0500_050_4911.RData effects_0500_025_4911.RData effects_1000_200_4911.RData \
	 effects_0500_100_4911.RData effects_1000_000_4911.RData effects_0500_150_4911.RData \
	 effects_1000_025_5323.RData effects_0500_025_5323.RData effects_0500_200_5323.RData \
	 effects_0500_050_5323.RData effects_1000_100_5323.RData effects_1000_150_5323.RData \
	 effects_0500_100_5323.RData effects_1000_050_5323.RData effects_1000_000_5323.RData \
	 effects_1000_200_5323.RData effects_0500_000_5323.RData effects_0500_150_5323.RData \
	 effects_0500_025_5462.RData effects_0500_050_5462.RData effects_1000_025_5462.RData \
	 effects_1000_050_5462.RData effects_0500_150_5462.RData effects_1000_100_5462.RData \
	 effects_1000_200_5462.RData effects_0500_000_5462.RData effects_1000_000_5462.RData \
	 effects_0500_200_5462.RData effects_0500_100_5462.RData effects_1000_150_5462.RData \
	 effects_0500_150_5472.RData effects_1000_150_5472.RData effects_1000_000_5472.RData \
	 effects_0500_100_5472.RData effects_1000_200_5472.RData effects_0500_050_5472.RData \
	 effects_0500_025_5472.RData effects_1000_050_5472.RData effects_1000_100_5472.RData \
	 effects_0500_000_5472.RData effects_0500_200_5472.RData effects_1000_025_5472.RData \
	 effects_0500_050_5916.RData effects_1000_150_5916.RData effects_0500_025_5916.RData \
	 effects_1000_025_5916.RData effects_1000_200_5916.RData effects_0500_100_5916.RData \
	 effects_1000_100_5916.RData effects_0500_200_5916.RData effects_0500_000_5916.RData \
	 effects_1000_000_5916.RData effects_0500_150_5916.RData effects_1000_050_5916.RData \
	 effects_1000_000_6158.RData effects_0500_100_6158.RData effects_0500_150_6158.RData \
	 effects_0500_200_6158.RData effects_1000_025_6158.RData effects_0500_025_6158.RData \
	 effects_1000_200_6158.RData effects_1000_150_6158.RData effects_1000_100_6158.RData \
	 effects_0500_000_6158.RData effects_1000_050_6158.RData effects_0500_050_6158.RData \
	 effects_0500_100_6181.RData effects_1000_025_6181.RData effects_0500_000_6181.RData \
	 effects_1000_200_6181.RData effects_0500_050_6181.RData effects_0500_150_6181.RData \
	 effects_0500_025_6181.RData effects_1000_150_6181.RData effects_0500_200_6181.RData \
	 effects_1000_000_6181.RData effects_1000_050_6181.RData effects_1000_100_6181.RData \
	 effects_0500_200_6207.RData effects_0500_150_6207.RData effects_1000_150_6207.RData \
	 effects_1000_050_6207.RData effects_0500_050_6207.RData effects_1000_100_6207.RData \
	 effects_0500_000_6207.RData effects_1000_025_6207.RData effects_0500_100_6207.RData \
	 effects_1000_200_6207.RData effects_1000_000_6207.RData effects_0500_025_6207.RData \
	 effects_0500_150_6574.RData effects_0500_000_6574.RData effects_1000_100_6574.RData \
	 effects_0500_050_6574.RData effects_0500_100_6574.RData effects_0500_025_6574.RData \
	 effects_1000_025_6574.RData effects_1000_000_6574.RData effects_1000_200_6574.RData \
	 effects_1000_050_6574.RData effects_1000_150_6574.RData effects_0500_200_6574.RData \
	 effects_0500_150_6581.RData effects_0500_100_6581.RData effects_0500_000_6581.RData \
	 effects_1000_000_6581.RData effects_1000_050_6581.RData effects_0500_200_6581.RData \
	 effects_1000_100_6581.RData effects_1000_025_6581.RData effects_1000_200_6581.RData \
	 effects_0500_050_6581.RData effects_1000_150_6581.RData effects_0500_025_6581.RData \
	 effects_0500_150_6742.RData effects_1000_025_6742.RData effects_1000_200_6742.RData \
	 effects_0500_100_6742.RData effects_0500_025_6742.RData effects_1000_100_6742.RData \
	 effects_1000_000_6742.RData effects_0500_000_6742.RData effects_0500_200_6742.RData \
	 effects_1000_050_6742.RData effects_1000_150_6742.RData effects_0500_050_6742.RData \
	 effects_0500_150_7442.RData effects_0500_050_7442.RData effects_1000_200_7442.RData \
	 effects_1000_000_7442.RData effects_1000_050_7442.RData effects_0500_000_7442.RData \
	 effects_1000_025_7442.RData effects_1000_100_7442.RData effects_0500_100_7442.RData \
	 effects_0500_200_7442.RData effects_1000_150_7442.RData effects_0500_025_7442.RData \
	 effects_0500_100_7758.RData effects_1000_200_7758.RData effects_1000_000_7758.RData \
	 effects_0500_150_7758.RData effects_1000_150_7758.RData effects_0500_050_7758.RData \
	 effects_0500_025_7758.RData effects_0500_200_7758.RData effects_1000_050_7758.RData \
	 effects_1000_025_7758.RData effects_0500_000_7758.RData effects_1000_100_7758.RData \
	 effects_1000_150_7768.RData effects_1000_000_7768.RData effects_0500_025_7768.RData \
	 effects_1000_100_7768.RData effects_0500_050_7768.RData effects_0500_150_7768.RData \
	 effects_0500_200_7768.RData effects_1000_050_7768.RData effects_0500_000_7768.RData \
	 effects_0500_100_7768.RData effects_1000_025_7768.RData effects_1000_200_7768.RData \
	 effects_1000_150_8245.RData effects_1000_100_8245.RData effects_0500_025_8245.RData \
	 effects_1000_025_8245.RData effects_1000_000_8245.RData effects_0500_000_8245.RData \
	 effects_0500_150_8245.RData effects_0500_200_8245.RData effects_1000_050_8245.RData \
	 effects_1000_200_8245.RData effects_0500_100_8245.RData effects_0500_050_8245.RData \
	 effects_0500_000_8264.RData effects_1000_050_8264.RData effects_0500_150_8264.RData \
	 effects_1000_000_8264.RData effects_0500_025_8264.RData effects_1000_150_8264.RData \
	 effects_0500_200_8264.RData effects_0500_050_8264.RData effects_1000_025_8264.RData \
	 effects_1000_200_8264.RData effects_1000_100_8264.RData effects_0500_100_8264.RData \
	 effects_1000_100_8424.RData effects_1000_150_8424.RData effects_0500_025_8424.RData \
	 effects_1000_025_8424.RData effects_1000_050_8424.RData effects_0500_050_8424.RData \
	 effects_0500_100_8424.RData effects_0500_200_8424.RData effects_1000_000_8424.RData \
	 effects_0500_000_8424.RData effects_0500_150_8424.RData effects_1000_200_8424.RData \
	 effects_0500_150_8553.RData effects_0500_050_8553.RData effects_1000_025_8553.RData \
	 effects_0500_200_8553.RData effects_1000_100_8553.RData effects_0500_000_8553.RData \
	 effects_1000_200_8553.RData effects_1000_000_8553.RData effects_1000_150_8553.RData \
	 effects_1000_050_8553.RData effects_0500_100_8553.RData effects_0500_025_8553.RData \
	 effects_0500_150_8714.RData effects_1000_000_8714.RData effects_1000_025_8714.RData \
	 effects_1000_050_8714.RData effects_0500_025_8714.RData effects_1000_150_8714.RData \
	 effects_0500_100_8714.RData effects_1000_200_8714.RData effects_0500_050_8714.RData \
	 effects_0500_000_8714.RData effects_0500_200_8714.RData effects_1000_100_8714.RData \
	 effects_1000_050_8841.RData effects_0500_200_8841.RData effects_0500_025_8841.RData \
	 effects_1000_000_8841.RData effects_0500_000_8841.RData effects_0500_050_8841.RData \
	 effects_1000_200_8841.RData effects_1000_025_8841.RData effects_1000_100_8841.RData \
	 effects_0500_150_8841.RData effects_1000_150_8841.RData effects_0500_100_8841.RData \
	 effects_0500_150_8921.RData effects_1000_025_8921.RData effects_1000_000_8921.RData \
	 effects_0500_000_8921.RData effects_0500_050_8921.RData effects_1000_050_8921.RData \
	 effects_1000_100_8921.RData effects_0500_200_8921.RData effects_0500_100_8921.RData \
	 effects_1000_150_8921.RData effects_0500_025_8921.RData effects_1000_200_8921.RData \
	 effects_0500_050_9132.RData effects_1000_100_9132.RData effects_1000_150_9132.RData \
	 effects_1000_000_9132.RData effects_1000_050_9132.RData effects_1000_200_9132.RData \
	 effects_0500_200_9132.RData effects_0500_025_9132.RData effects_0500_000_9132.RData \
	 effects_1000_025_9132.RData effects_0500_150_9132.RData effects_0500_100_9132.RData \
	 effects_1000_200_9189.RData effects_0500_100_9189.RData effects_0500_000_9189.RData \
	 effects_0500_200_9189.RData effects_1000_100_9189.RData effects_1000_025_9189.RData \
	 effects_0500_150_9189.RData effects_0500_025_9189.RData effects_0500_050_9189.RData \
	 effects_1000_000_9189.RData effects_1000_050_9189.RData effects_1000_150_9189.RData \
	 effects_1000_000_9459.RData effects_1000_025_9459.RData effects_1000_050_9459.RData \
	 effects_0500_150_9459.RData effects_0500_200_9459.RData effects_0500_050_9459.RData \
	 effects_1000_150_9459.RData effects_0500_100_9459.RData effects_1000_200_9459.RData \
	 effects_0500_025_9459.RData effects_1000_100_9459.RData effects_0500_000_9459.RData \
	 effects_1000_050_9697.RData effects_0500_050_9697.RData effects_1000_100_9697.RData \
	 effects_0500_100_9697.RData effects_1000_000_9697.RData effects_0500_200_9697.RData \
	 effects_0500_150_9697.RData effects_1000_150_9697.RData effects_1000_200_9697.RData \
	 effects_0500_000_9697.RData effects_1000_025_9697.RData effects_0500_025_9697.RData \
	 effects_1000_050_9795.RData effects_0500_100_9795.RData effects_1000_000_9795.RData \
	 effects_0500_150_9795.RData effects_0500_050_9795.RData effects_1000_200_9795.RData \
	 effects_1000_150_9795.RData effects_1000_025_9795.RData effects_1000_100_9795.RData \
	 effects_0500_000_9795.RData effects_0500_200_9795.RData effects_0500_025_9795.RData \
	 effects_1000_000_9968.RData effects_0500_200_9968.RData effects_0500_000_9968.RData \
	 effects_0500_150_9968.RData effects_1000_050_9968.RData effects_1000_150_9968.RData \
	 effects_1000_100_9968.RData effects_0500_050_9968.RData effects_0500_025_9968.RData \
	 effects_0500_100_9968.RData effects_1000_200_9968.RData effects_1000_025_9968.RData  

 effects_0500_100_0013.RData: effects_0500_100_0013.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_0013.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_0013.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_0013.R'

effects_1000_050_0013.RData: effects_1000_050_0013.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_0013.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_0013.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_0013.R'

effects_1000_000_0013.RData: effects_1000_000_0013.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_0013.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_0013.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_0013.R'

effects_1000_100_0013.RData: effects_1000_100_0013.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_0013.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_0013.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_0013.R'

effects_1000_025_0013.RData: effects_1000_025_0013.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_0013.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_0013.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_0013.R'

effects_0500_050_0013.RData: effects_0500_050_0013.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_0013.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_0013.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_0013.R'

effects_0500_200_0013.RData: effects_0500_200_0013.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_0013.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_0013.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_0013.R'

effects_1000_200_0013.RData: effects_1000_200_0013.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_0013.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_0013.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_0013.R'

effects_0500_000_0013.RData: effects_0500_000_0013.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_0013.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_0013.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_0013.R'

effects_1000_150_0013.RData: effects_1000_150_0013.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_0013.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_0013.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_0013.R'

effects_0500_150_0013.RData: effects_0500_150_0013.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_0013.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_0013.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_0013.R'

effects_0500_025_0013.RData: effects_0500_025_0013.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_0013.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_0013.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_0013.R'

effects_0500_100_0156.RData: effects_0500_100_0156.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_0156.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_0156.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_0156.R'

effects_1000_150_0156.RData: effects_1000_150_0156.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_0156.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_0156.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_0156.R'

effects_0500_025_0156.RData: effects_0500_025_0156.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_0156.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_0156.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_0156.R'

effects_0500_050_0156.RData: effects_0500_050_0156.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_0156.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_0156.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_0156.R'

effects_1000_000_0156.RData: effects_1000_000_0156.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_0156.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_0156.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_0156.R'

effects_0500_200_0156.RData: effects_0500_200_0156.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_0156.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_0156.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_0156.R'

effects_1000_025_0156.RData: effects_1000_025_0156.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_0156.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_0156.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_0156.R'

effects_0500_000_0156.RData: effects_0500_000_0156.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_0156.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_0156.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_0156.R'

effects_1000_100_0156.RData: effects_1000_100_0156.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_0156.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_0156.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_0156.R'

effects_1000_050_0156.RData: effects_1000_050_0156.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_0156.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_0156.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_0156.R'

effects_0500_150_0156.RData: effects_0500_150_0156.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_0156.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_0156.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_0156.R'

effects_1000_200_0156.RData: effects_1000_200_0156.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_0156.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_0156.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_0156.R'

effects_1000_150_0523.RData: effects_1000_150_0523.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_0523.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_0523.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_0523.R'

effects_0500_150_0523.RData: effects_0500_150_0523.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_0523.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_0523.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_0523.R'

effects_0500_000_0523.RData: effects_0500_000_0523.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_0523.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_0523.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_0523.R'

effects_1000_100_0523.RData: effects_1000_100_0523.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_0523.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_0523.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_0523.R'

effects_1000_025_0523.RData: effects_1000_025_0523.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_0523.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_0523.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_0523.R'

effects_0500_025_0523.RData: effects_0500_025_0523.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_0523.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_0523.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_0523.R'

effects_0500_100_0523.RData: effects_0500_100_0523.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_0523.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_0523.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_0523.R'

effects_1000_000_0523.RData: effects_1000_000_0523.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_0523.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_0523.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_0523.R'

effects_0500_200_0523.RData: effects_0500_200_0523.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_0523.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_0523.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_0523.R'

effects_0500_050_0523.RData: effects_0500_050_0523.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_0523.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_0523.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_0523.R'

effects_1000_200_0523.RData: effects_1000_200_0523.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_0523.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_0523.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_0523.R'

effects_1000_050_0523.RData: effects_1000_050_0523.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_0523.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_0523.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_0523.R'

effects_1000_025_0666.RData: effects_1000_025_0666.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_0666.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_0666.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_0666.R'

effects_1000_000_0666.RData: effects_1000_000_0666.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_0666.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_0666.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_0666.R'

effects_1000_050_0666.RData: effects_1000_050_0666.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_0666.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_0666.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_0666.R'

effects_1000_100_0666.RData: effects_1000_100_0666.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_0666.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_0666.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_0666.R'

effects_0500_050_0666.RData: effects_0500_050_0666.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_0666.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_0666.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_0666.R'

effects_0500_025_0666.RData: effects_0500_025_0666.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_0666.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_0666.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_0666.R'

effects_0500_100_0666.RData: effects_0500_100_0666.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_0666.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_0666.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_0666.R'

effects_0500_150_0666.RData: effects_0500_150_0666.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_0666.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_0666.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_0666.R'

effects_0500_200_0666.RData: effects_0500_200_0666.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_0666.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_0666.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_0666.R'

effects_1000_150_0666.RData: effects_1000_150_0666.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_0666.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_0666.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_0666.R'

effects_1000_200_0666.RData: effects_1000_200_0666.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_0666.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_0666.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_0666.R'

effects_0500_000_0666.RData: effects_0500_000_0666.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_0666.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_0666.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_0666.R'

effects_0500_200_0804.RData: effects_0500_200_0804.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_0804.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_0804.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_0804.R'

effects_1000_025_0804.RData: effects_1000_025_0804.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_0804.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_0804.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_0804.R'

effects_0500_100_0804.RData: effects_0500_100_0804.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_0804.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_0804.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_0804.R'

effects_0500_150_0804.RData: effects_0500_150_0804.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_0804.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_0804.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_0804.R'

effects_1000_200_0804.RData: effects_1000_200_0804.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_0804.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_0804.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_0804.R'

effects_1000_100_0804.RData: effects_1000_100_0804.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_0804.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_0804.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_0804.R'

effects_1000_150_0804.RData: effects_1000_150_0804.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_0804.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_0804.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_0804.R'

effects_1000_000_0804.RData: effects_1000_000_0804.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_0804.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_0804.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_0804.R'

effects_0500_050_0804.RData: effects_0500_050_0804.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_0804.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_0804.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_0804.R'

effects_0500_000_0804.RData: effects_0500_000_0804.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_0804.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_0804.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_0804.R'

effects_0500_025_0804.RData: effects_0500_025_0804.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_0804.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_0804.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_0804.R'

effects_1000_050_0804.RData: effects_1000_050_0804.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_0804.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_0804.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_0804.R'

effects_1000_100_0871.RData: effects_1000_100_0871.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_0871.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_0871.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_0871.R'

effects_0500_000_0871.RData: effects_0500_000_0871.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_0871.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_0871.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_0871.R'

effects_0500_050_0871.RData: effects_0500_050_0871.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_0871.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_0871.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_0871.R'

effects_0500_100_0871.RData: effects_0500_100_0871.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_0871.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_0871.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_0871.R'

effects_1000_200_0871.RData: effects_1000_200_0871.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_0871.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_0871.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_0871.R'

effects_1000_050_0871.RData: effects_1000_050_0871.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_0871.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_0871.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_0871.R'

effects_0500_025_0871.RData: effects_0500_025_0871.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_0871.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_0871.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_0871.R'

effects_1000_150_0871.RData: effects_1000_150_0871.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_0871.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_0871.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_0871.R'

effects_1000_000_0871.RData: effects_1000_000_0871.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_0871.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_0871.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_0871.R'

effects_1000_025_0871.RData: effects_1000_025_0871.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_0871.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_0871.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_0871.R'

effects_0500_150_0871.RData: effects_0500_150_0871.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_0871.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_0871.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_0871.R'

effects_0500_200_0871.RData: effects_0500_200_0871.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_0871.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_0871.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_0871.R'

effects_0500_025_1030.RData: effects_0500_025_1030.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_1030.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_1030.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_1030.R'

effects_0500_100_1030.RData: effects_0500_100_1030.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_1030.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_1030.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_1030.R'

effects_1000_100_1030.RData: effects_1000_100_1030.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_1030.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_1030.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_1030.R'

effects_0500_000_1030.RData: effects_0500_000_1030.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_1030.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_1030.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_1030.R'

effects_0500_200_1030.RData: effects_0500_200_1030.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_1030.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_1030.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_1030.R'

effects_1000_050_1030.RData: effects_1000_050_1030.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_1030.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_1030.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_1030.R'

effects_1000_150_1030.RData: effects_1000_150_1030.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_1030.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_1030.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_1030.R'

effects_0500_050_1030.RData: effects_0500_050_1030.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_1030.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_1030.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_1030.R'

effects_1000_000_1030.RData: effects_1000_000_1030.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_1030.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_1030.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_1030.R'

effects_1000_025_1030.RData: effects_1000_025_1030.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_1030.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_1030.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_1030.R'

effects_1000_200_1030.RData: effects_1000_200_1030.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_1030.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_1030.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_1030.R'

effects_0500_150_1030.RData: effects_0500_150_1030.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_1030.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_1030.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_1030.R'

effects_1000_025_1376.RData: effects_1000_025_1376.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_1376.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_1376.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_1376.R'

effects_1000_050_1376.RData: effects_1000_050_1376.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_1376.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_1376.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_1376.R'

effects_1000_000_1376.RData: effects_1000_000_1376.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_1376.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_1376.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_1376.R'

effects_0500_050_1376.RData: effects_0500_050_1376.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_1376.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_1376.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_1376.R'

effects_1000_150_1376.RData: effects_1000_150_1376.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_1376.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_1376.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_1376.R'

effects_0500_000_1376.RData: effects_0500_000_1376.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_1376.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_1376.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_1376.R'

effects_0500_100_1376.RData: effects_0500_100_1376.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_1376.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_1376.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_1376.R'

effects_1000_200_1376.RData: effects_1000_200_1376.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_1376.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_1376.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_1376.R'

effects_0500_025_1376.RData: effects_0500_025_1376.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_1376.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_1376.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_1376.R'

effects_0500_150_1376.RData: effects_0500_150_1376.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_1376.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_1376.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_1376.R'

effects_1000_100_1376.RData: effects_1000_100_1376.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_1376.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_1376.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_1376.R'

effects_0500_200_1376.RData: effects_0500_200_1376.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_1376.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_1376.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_1376.R'

effects_0500_100_1571.RData: effects_0500_100_1571.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_1571.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_1571.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_1571.R'

effects_1000_025_1571.RData: effects_1000_025_1571.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_1571.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_1571.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_1571.R'

effects_1000_150_1571.RData: effects_1000_150_1571.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_1571.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_1571.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_1571.R'

effects_1000_050_1571.RData: effects_1000_050_1571.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_1571.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_1571.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_1571.R'

effects_0500_200_1571.RData: effects_0500_200_1571.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_1571.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_1571.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_1571.R'

effects_0500_000_1571.RData: effects_0500_000_1571.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_1571.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_1571.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_1571.R'

effects_0500_150_1571.RData: effects_0500_150_1571.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_1571.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_1571.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_1571.R'

effects_0500_050_1571.RData: effects_0500_050_1571.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_1571.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_1571.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_1571.R'

effects_0500_025_1571.RData: effects_0500_025_1571.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_1571.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_1571.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_1571.R'

effects_1000_200_1571.RData: effects_1000_200_1571.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_1571.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_1571.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_1571.R'

effects_1000_000_1571.RData: effects_1000_000_1571.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_1571.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_1571.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_1571.R'

effects_1000_100_1571.RData: effects_1000_100_1571.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_1571.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_1571.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_1571.R'

effects_1000_025_1715.RData: effects_1000_025_1715.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_1715.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_1715.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_1715.R'

effects_1000_150_1715.RData: effects_1000_150_1715.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_1715.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_1715.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_1715.R'

effects_1000_000_1715.RData: effects_1000_000_1715.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_1715.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_1715.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_1715.R'

effects_0500_000_1715.RData: effects_0500_000_1715.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_1715.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_1715.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_1715.R'

effects_0500_100_1715.RData: effects_0500_100_1715.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_1715.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_1715.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_1715.R'

effects_1000_100_1715.RData: effects_1000_100_1715.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_1715.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_1715.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_1715.R'

effects_0500_025_1715.RData: effects_0500_025_1715.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_1715.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_1715.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_1715.R'

effects_1000_200_1715.RData: effects_1000_200_1715.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_1715.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_1715.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_1715.R'

effects_1000_050_1715.RData: effects_1000_050_1715.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_1715.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_1715.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_1715.R'

effects_0500_050_1715.RData: effects_0500_050_1715.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_1715.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_1715.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_1715.R'

effects_0500_200_1715.RData: effects_0500_200_1715.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_1715.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_1715.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_1715.R'

effects_0500_150_1715.RData: effects_0500_150_1715.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_1715.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_1715.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_1715.R'

effects_0500_150_1925.RData: effects_0500_150_1925.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_1925.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_1925.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_1925.R'

effects_1000_050_1925.RData: effects_1000_050_1925.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_1925.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_1925.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_1925.R'

effects_0500_200_1925.RData: effects_0500_200_1925.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_1925.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_1925.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_1925.R'

effects_0500_100_1925.RData: effects_0500_100_1925.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_1925.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_1925.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_1925.R'

effects_1000_200_1925.RData: effects_1000_200_1925.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_1925.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_1925.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_1925.R'

effects_0500_000_1925.RData: effects_0500_000_1925.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_1925.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_1925.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_1925.R'

effects_1000_100_1925.RData: effects_1000_100_1925.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_1925.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_1925.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_1925.R'

effects_1000_150_1925.RData: effects_1000_150_1925.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_1925.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_1925.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_1925.R'

effects_1000_025_1925.RData: effects_1000_025_1925.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_1925.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_1925.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_1925.R'

effects_0500_050_1925.RData: effects_0500_050_1925.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_1925.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_1925.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_1925.R'

effects_1000_000_1925.RData: effects_1000_000_1925.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_1925.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_1925.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_1925.R'

effects_0500_025_1925.RData: effects_0500_025_1925.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_1925.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_1925.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_1925.R'

effects_1000_025_2110.RData: effects_1000_025_2110.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_2110.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_2110.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_2110.R'

effects_1000_000_2110.RData: effects_1000_000_2110.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_2110.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_2110.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_2110.R'

effects_0500_025_2110.RData: effects_0500_025_2110.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_2110.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_2110.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_2110.R'

effects_0500_150_2110.RData: effects_0500_150_2110.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_2110.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_2110.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_2110.R'

effects_1000_200_2110.RData: effects_1000_200_2110.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_2110.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_2110.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_2110.R'

effects_0500_200_2110.RData: effects_0500_200_2110.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_2110.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_2110.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_2110.R'

effects_1000_050_2110.RData: effects_1000_050_2110.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_2110.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_2110.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_2110.R'

effects_0500_050_2110.RData: effects_0500_050_2110.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_2110.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_2110.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_2110.R'

effects_1000_150_2110.RData: effects_1000_150_2110.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_2110.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_2110.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_2110.R'

effects_1000_100_2110.RData: effects_1000_100_2110.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_2110.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_2110.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_2110.R'

effects_0500_100_2110.RData: effects_0500_100_2110.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_2110.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_2110.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_2110.R'

effects_0500_000_2110.RData: effects_0500_000_2110.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_2110.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_2110.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_2110.R'

effects_1000_200_2222.RData: effects_1000_200_2222.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_2222.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_2222.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_2222.R'

effects_1000_025_2222.RData: effects_1000_025_2222.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_2222.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_2222.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_2222.R'

effects_1000_050_2222.RData: effects_1000_050_2222.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_2222.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_2222.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_2222.R'

effects_0500_025_2222.RData: effects_0500_025_2222.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_2222.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_2222.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_2222.R'

effects_1000_000_2222.RData: effects_1000_000_2222.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_2222.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_2222.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_2222.R'

effects_0500_200_2222.RData: effects_0500_200_2222.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_2222.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_2222.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_2222.R'

effects_1000_100_2222.RData: effects_1000_100_2222.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_2222.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_2222.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_2222.R'

effects_0500_150_2222.RData: effects_0500_150_2222.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_2222.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_2222.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_2222.R'

effects_1000_150_2222.RData: effects_1000_150_2222.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_2222.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_2222.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_2222.R'

effects_0500_050_2222.RData: effects_0500_050_2222.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_2222.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_2222.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_2222.R'

effects_0500_100_2222.RData: effects_0500_100_2222.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_2222.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_2222.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_2222.R'

effects_0500_000_2222.RData: effects_0500_000_2222.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_2222.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_2222.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_2222.R'

effects_0500_050_2224.RData: effects_0500_050_2224.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_2224.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_2224.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_2224.R'

effects_1000_000_2224.RData: effects_1000_000_2224.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_2224.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_2224.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_2224.R'

effects_0500_150_2224.RData: effects_0500_150_2224.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_2224.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_2224.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_2224.R'

effects_0500_100_2224.RData: effects_0500_100_2224.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_2224.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_2224.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_2224.R'

effects_1000_025_2224.RData: effects_1000_025_2224.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_2224.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_2224.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_2224.R'

effects_0500_200_2224.RData: effects_0500_200_2224.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_2224.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_2224.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_2224.R'

effects_1000_100_2224.RData: effects_1000_100_2224.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_2224.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_2224.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_2224.R'

effects_1000_200_2224.RData: effects_1000_200_2224.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_2224.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_2224.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_2224.R'

effects_1000_050_2224.RData: effects_1000_050_2224.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_2224.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_2224.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_2224.R'

effects_0500_025_2224.RData: effects_0500_025_2224.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_2224.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_2224.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_2224.R'

effects_0500_000_2224.RData: effects_0500_000_2224.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_2224.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_2224.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_2224.R'

effects_1000_150_2224.RData: effects_1000_150_2224.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_2224.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_2224.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_2224.R'

effects_0500_200_3297.RData: effects_0500_200_3297.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_3297.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_3297.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_3297.R'

effects_1000_050_3297.RData: effects_1000_050_3297.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_3297.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_3297.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_3297.R'

effects_0500_050_3297.RData: effects_0500_050_3297.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_3297.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_3297.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_3297.R'

effects_0500_150_3297.RData: effects_0500_150_3297.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_3297.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_3297.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_3297.R'

effects_1000_025_3297.RData: effects_1000_025_3297.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_3297.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_3297.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_3297.R'

effects_1000_200_3297.RData: effects_1000_200_3297.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_3297.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_3297.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_3297.R'

effects_1000_100_3297.RData: effects_1000_100_3297.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_3297.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_3297.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_3297.R'

effects_1000_150_3297.RData: effects_1000_150_3297.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_3297.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_3297.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_3297.R'

effects_0500_025_3297.RData: effects_0500_025_3297.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_3297.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_3297.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_3297.R'

effects_1000_000_3297.RData: effects_1000_000_3297.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_3297.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_3297.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_3297.R'

effects_0500_100_3297.RData: effects_0500_100_3297.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_3297.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_3297.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_3297.R'

effects_0500_000_3297.RData: effects_0500_000_3297.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_3297.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_3297.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_3297.R'

effects_0500_025_3471.RData: effects_0500_025_3471.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_3471.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_3471.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_3471.R'

effects_1000_150_3471.RData: effects_1000_150_3471.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_3471.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_3471.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_3471.R'

effects_0500_050_3471.RData: effects_0500_050_3471.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_3471.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_3471.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_3471.R'

effects_1000_000_3471.RData: effects_1000_000_3471.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_3471.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_3471.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_3471.R'

effects_1000_100_3471.RData: effects_1000_100_3471.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_3471.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_3471.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_3471.R'

effects_1000_050_3471.RData: effects_1000_050_3471.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_3471.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_3471.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_3471.R'

effects_0500_150_3471.RData: effects_0500_150_3471.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_3471.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_3471.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_3471.R'

effects_0500_200_3471.RData: effects_0500_200_3471.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_3471.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_3471.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_3471.R'

effects_1000_200_3471.RData: effects_1000_200_3471.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_3471.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_3471.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_3471.R'

effects_0500_000_3471.RData: effects_0500_000_3471.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_3471.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_3471.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_3471.R'

effects_0500_100_3471.RData: effects_0500_100_3471.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_3471.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_3471.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_3471.R'

effects_1000_025_3471.RData: effects_1000_025_3471.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_3471.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_3471.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_3471.R'

effects_0500_025_3621.RData: effects_0500_025_3621.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_3621.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_3621.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_3621.R'

effects_0500_200_3621.RData: effects_0500_200_3621.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_3621.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_3621.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_3621.R'

effects_0500_150_3621.RData: effects_0500_150_3621.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_3621.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_3621.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_3621.R'

effects_1000_150_3621.RData: effects_1000_150_3621.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_3621.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_3621.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_3621.R'

effects_1000_025_3621.RData: effects_1000_025_3621.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_3621.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_3621.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_3621.R'

effects_0500_000_3621.RData: effects_0500_000_3621.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_3621.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_3621.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_3621.R'

effects_0500_100_3621.RData: effects_0500_100_3621.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_3621.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_3621.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_3621.R'

effects_1000_200_3621.RData: effects_1000_200_3621.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_3621.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_3621.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_3621.R'

effects_1000_100_3621.RData: effects_1000_100_3621.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_3621.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_3621.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_3621.R'

effects_0500_050_3621.RData: effects_0500_050_3621.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_3621.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_3621.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_3621.R'

effects_1000_000_3621.RData: effects_1000_000_3621.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_3621.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_3621.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_3621.R'

effects_1000_050_3621.RData: effects_1000_050_3621.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_3621.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_3621.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_3621.R'

effects_0500_050_3666.RData: effects_0500_050_3666.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_3666.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_3666.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_3666.R'

effects_0500_025_3666.RData: effects_0500_025_3666.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_3666.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_3666.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_3666.R'

effects_1000_025_3666.RData: effects_1000_025_3666.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_3666.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_3666.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_3666.R'

effects_0500_200_3666.RData: effects_0500_200_3666.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_3666.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_3666.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_3666.R'

effects_1000_150_3666.RData: effects_1000_150_3666.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_3666.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_3666.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_3666.R'

effects_1000_100_3666.RData: effects_1000_100_3666.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_3666.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_3666.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_3666.R'

effects_1000_000_3666.RData: effects_1000_000_3666.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_3666.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_3666.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_3666.R'

effects_0500_150_3666.RData: effects_0500_150_3666.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_3666.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_3666.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_3666.R'

effects_1000_200_3666.RData: effects_1000_200_3666.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_3666.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_3666.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_3666.R'

effects_0500_100_3666.RData: effects_0500_100_3666.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_3666.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_3666.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_3666.R'

effects_1000_050_3666.RData: effects_1000_050_3666.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_3666.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_3666.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_3666.R'

effects_0500_000_3666.RData: effects_0500_000_3666.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_3666.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_3666.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_3666.R'

effects_0500_150_4227.RData: effects_0500_150_4227.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_4227.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_4227.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_4227.R'

effects_0500_000_4227.RData: effects_0500_000_4227.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_4227.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_4227.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_4227.R'

effects_1000_025_4227.RData: effects_1000_025_4227.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_4227.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_4227.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_4227.R'

effects_0500_200_4227.RData: effects_0500_200_4227.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_4227.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_4227.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_4227.R'

effects_1000_000_4227.RData: effects_1000_000_4227.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_4227.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_4227.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_4227.R'

effects_1000_050_4227.RData: effects_1000_050_4227.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_4227.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_4227.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_4227.R'

effects_1000_150_4227.RData: effects_1000_150_4227.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_4227.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_4227.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_4227.R'

effects_1000_200_4227.RData: effects_1000_200_4227.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_4227.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_4227.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_4227.R'

effects_0500_100_4227.RData: effects_0500_100_4227.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_4227.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_4227.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_4227.R'

effects_0500_025_4227.RData: effects_0500_025_4227.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_4227.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_4227.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_4227.R'

effects_1000_100_4227.RData: effects_1000_100_4227.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_4227.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_4227.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_4227.R'

effects_0500_050_4227.RData: effects_0500_050_4227.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_4227.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_4227.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_4227.R'

effects_1000_050_4378.RData: effects_1000_050_4378.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_4378.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_4378.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_4378.R'

effects_0500_050_4378.RData: effects_0500_050_4378.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_4378.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_4378.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_4378.R'

effects_0500_150_4378.RData: effects_0500_150_4378.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_4378.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_4378.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_4378.R'

effects_1000_025_4378.RData: effects_1000_025_4378.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_4378.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_4378.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_4378.R'

effects_1000_000_4378.RData: effects_1000_000_4378.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_4378.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_4378.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_4378.R'

effects_0500_000_4378.RData: effects_0500_000_4378.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_4378.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_4378.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_4378.R'

effects_0500_100_4378.RData: effects_0500_100_4378.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_4378.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_4378.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_4378.R'

effects_1000_150_4378.RData: effects_1000_150_4378.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_4378.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_4378.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_4378.R'

effects_0500_025_4378.RData: effects_0500_025_4378.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_4378.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_4378.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_4378.R'

effects_0500_200_4378.RData: effects_0500_200_4378.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_4378.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_4378.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_4378.R'

effects_1000_200_4378.RData: effects_1000_200_4378.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_4378.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_4378.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_4378.R'

effects_1000_100_4378.RData: effects_1000_100_4378.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_4378.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_4378.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_4378.R'

effects_1000_150_4399.RData: effects_1000_150_4399.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_4399.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_4399.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_4399.R'

effects_1000_100_4399.RData: effects_1000_100_4399.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_4399.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_4399.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_4399.R'

effects_0500_100_4399.RData: effects_0500_100_4399.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_4399.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_4399.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_4399.R'

effects_0500_050_4399.RData: effects_0500_050_4399.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_4399.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_4399.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_4399.R'

effects_0500_000_4399.RData: effects_0500_000_4399.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_4399.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_4399.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_4399.R'

effects_1000_050_4399.RData: effects_1000_050_4399.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_4399.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_4399.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_4399.R'

effects_0500_200_4399.RData: effects_0500_200_4399.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_4399.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_4399.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_4399.R'

effects_0500_150_4399.RData: effects_0500_150_4399.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_4399.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_4399.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_4399.R'

effects_1000_025_4399.RData: effects_1000_025_4399.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_4399.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_4399.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_4399.R'

effects_1000_000_4399.RData: effects_1000_000_4399.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_4399.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_4399.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_4399.R'

effects_0500_025_4399.RData: effects_0500_025_4399.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_4399.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_4399.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_4399.R'

effects_1000_200_4399.RData: effects_1000_200_4399.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_4399.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_4399.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_4399.R'

effects_0500_100_4472.RData: effects_0500_100_4472.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_4472.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_4472.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_4472.R'

effects_0500_150_4472.RData: effects_0500_150_4472.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_4472.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_4472.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_4472.R'

effects_1000_000_4472.RData: effects_1000_000_4472.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_4472.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_4472.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_4472.R'

effects_1000_025_4472.RData: effects_1000_025_4472.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_4472.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_4472.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_4472.R'

effects_1000_050_4472.RData: effects_1000_050_4472.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_4472.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_4472.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_4472.R'

effects_1000_150_4472.RData: effects_1000_150_4472.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_4472.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_4472.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_4472.R'

effects_0500_025_4472.RData: effects_0500_025_4472.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_4472.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_4472.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_4472.R'

effects_0500_050_4472.RData: effects_0500_050_4472.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_4472.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_4472.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_4472.R'

effects_0500_200_4472.RData: effects_0500_200_4472.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_4472.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_4472.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_4472.R'

effects_1000_200_4472.RData: effects_1000_200_4472.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_4472.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_4472.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_4472.R'

effects_0500_000_4472.RData: effects_0500_000_4472.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_4472.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_4472.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_4472.R'

effects_1000_100_4472.RData: effects_1000_100_4472.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_4472.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_4472.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_4472.R'

effects_1000_200_4824.RData: effects_1000_200_4824.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_4824.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_4824.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_4824.R'

effects_1000_000_4824.RData: effects_1000_000_4824.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_4824.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_4824.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_4824.R'

effects_0500_000_4824.RData: effects_0500_000_4824.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_4824.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_4824.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_4824.R'

effects_1000_050_4824.RData: effects_1000_050_4824.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_4824.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_4824.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_4824.R'

effects_1000_025_4824.RData: effects_1000_025_4824.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_4824.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_4824.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_4824.R'

effects_0500_150_4824.RData: effects_0500_150_4824.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_4824.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_4824.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_4824.R'

effects_1000_150_4824.RData: effects_1000_150_4824.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_4824.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_4824.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_4824.R'

effects_1000_100_4824.RData: effects_1000_100_4824.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_4824.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_4824.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_4824.R'

effects_0500_050_4824.RData: effects_0500_050_4824.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_4824.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_4824.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_4824.R'

effects_0500_200_4824.RData: effects_0500_200_4824.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_4824.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_4824.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_4824.R'

effects_0500_025_4824.RData: effects_0500_025_4824.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_4824.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_4824.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_4824.R'

effects_0500_100_4824.RData: effects_0500_100_4824.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_4824.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_4824.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_4824.R'

effects_1000_050_4911.RData: effects_1000_050_4911.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_4911.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_4911.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_4911.R'

effects_0500_000_4911.RData: effects_0500_000_4911.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_4911.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_4911.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_4911.R'

effects_1000_100_4911.RData: effects_1000_100_4911.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_4911.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_4911.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_4911.R'

effects_0500_200_4911.RData: effects_0500_200_4911.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_4911.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_4911.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_4911.R'

effects_1000_150_4911.RData: effects_1000_150_4911.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_4911.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_4911.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_4911.R'

effects_1000_025_4911.RData: effects_1000_025_4911.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_4911.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_4911.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_4911.R'

effects_0500_050_4911.RData: effects_0500_050_4911.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_4911.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_4911.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_4911.R'

effects_0500_025_4911.RData: effects_0500_025_4911.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_4911.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_4911.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_4911.R'

effects_1000_200_4911.RData: effects_1000_200_4911.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_4911.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_4911.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_4911.R'

effects_0500_100_4911.RData: effects_0500_100_4911.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_4911.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_4911.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_4911.R'

effects_1000_000_4911.RData: effects_1000_000_4911.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_4911.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_4911.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_4911.R'

effects_0500_150_4911.RData: effects_0500_150_4911.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_4911.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_4911.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_4911.R'

effects_1000_025_5323.RData: effects_1000_025_5323.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_5323.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_5323.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_5323.R'

effects_0500_025_5323.RData: effects_0500_025_5323.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_5323.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_5323.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_5323.R'

effects_0500_200_5323.RData: effects_0500_200_5323.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_5323.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_5323.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_5323.R'

effects_0500_050_5323.RData: effects_0500_050_5323.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_5323.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_5323.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_5323.R'

effects_1000_100_5323.RData: effects_1000_100_5323.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_5323.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_5323.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_5323.R'

effects_1000_150_5323.RData: effects_1000_150_5323.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_5323.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_5323.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_5323.R'

effects_0500_100_5323.RData: effects_0500_100_5323.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_5323.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_5323.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_5323.R'

effects_1000_050_5323.RData: effects_1000_050_5323.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_5323.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_5323.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_5323.R'

effects_1000_000_5323.RData: effects_1000_000_5323.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_5323.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_5323.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_5323.R'

effects_1000_200_5323.RData: effects_1000_200_5323.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_5323.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_5323.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_5323.R'

effects_0500_000_5323.RData: effects_0500_000_5323.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_5323.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_5323.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_5323.R'

effects_0500_150_5323.RData: effects_0500_150_5323.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_5323.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_5323.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_5323.R'

effects_0500_025_5462.RData: effects_0500_025_5462.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_5462.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_5462.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_5462.R'

effects_0500_050_5462.RData: effects_0500_050_5462.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_5462.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_5462.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_5462.R'

effects_1000_025_5462.RData: effects_1000_025_5462.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_5462.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_5462.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_5462.R'

effects_1000_050_5462.RData: effects_1000_050_5462.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_5462.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_5462.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_5462.R'

effects_0500_150_5462.RData: effects_0500_150_5462.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_5462.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_5462.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_5462.R'

effects_1000_100_5462.RData: effects_1000_100_5462.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_5462.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_5462.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_5462.R'

effects_1000_200_5462.RData: effects_1000_200_5462.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_5462.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_5462.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_5462.R'

effects_0500_000_5462.RData: effects_0500_000_5462.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_5462.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_5462.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_5462.R'

effects_1000_000_5462.RData: effects_1000_000_5462.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_5462.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_5462.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_5462.R'

effects_0500_200_5462.RData: effects_0500_200_5462.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_5462.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_5462.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_5462.R'

effects_0500_100_5462.RData: effects_0500_100_5462.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_5462.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_5462.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_5462.R'

effects_1000_150_5462.RData: effects_1000_150_5462.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_5462.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_5462.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_5462.R'

effects_0500_150_5472.RData: effects_0500_150_5472.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_5472.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_5472.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_5472.R'

effects_1000_150_5472.RData: effects_1000_150_5472.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_5472.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_5472.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_5472.R'

effects_1000_000_5472.RData: effects_1000_000_5472.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_5472.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_5472.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_5472.R'

effects_0500_100_5472.RData: effects_0500_100_5472.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_5472.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_5472.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_5472.R'

effects_1000_200_5472.RData: effects_1000_200_5472.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_5472.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_5472.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_5472.R'

effects_0500_050_5472.RData: effects_0500_050_5472.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_5472.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_5472.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_5472.R'

effects_0500_025_5472.RData: effects_0500_025_5472.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_5472.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_5472.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_5472.R'

effects_1000_050_5472.RData: effects_1000_050_5472.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_5472.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_5472.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_5472.R'

effects_1000_100_5472.RData: effects_1000_100_5472.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_5472.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_5472.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_5472.R'

effects_0500_000_5472.RData: effects_0500_000_5472.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_5472.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_5472.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_5472.R'

effects_0500_200_5472.RData: effects_0500_200_5472.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_5472.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_5472.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_5472.R'

effects_1000_025_5472.RData: effects_1000_025_5472.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_5472.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_5472.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_5472.R'

effects_0500_050_5916.RData: effects_0500_050_5916.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_5916.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_5916.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_5916.R'

effects_1000_150_5916.RData: effects_1000_150_5916.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_5916.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_5916.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_5916.R'

effects_0500_025_5916.RData: effects_0500_025_5916.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_5916.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_5916.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_5916.R'

effects_1000_025_5916.RData: effects_1000_025_5916.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_5916.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_5916.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_5916.R'

effects_1000_200_5916.RData: effects_1000_200_5916.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_5916.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_5916.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_5916.R'

effects_0500_100_5916.RData: effects_0500_100_5916.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_5916.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_5916.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_5916.R'

effects_1000_100_5916.RData: effects_1000_100_5916.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_5916.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_5916.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_5916.R'

effects_0500_200_5916.RData: effects_0500_200_5916.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_5916.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_5916.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_5916.R'

effects_0500_000_5916.RData: effects_0500_000_5916.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_5916.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_5916.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_5916.R'

effects_1000_000_5916.RData: effects_1000_000_5916.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_5916.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_5916.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_5916.R'

effects_0500_150_5916.RData: effects_0500_150_5916.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_5916.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_5916.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_5916.R'

effects_1000_050_5916.RData: effects_1000_050_5916.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_5916.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_5916.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_5916.R'

effects_1000_000_6158.RData: effects_1000_000_6158.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_6158.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_6158.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_6158.R'

effects_0500_100_6158.RData: effects_0500_100_6158.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_6158.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_6158.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_6158.R'

effects_0500_150_6158.RData: effects_0500_150_6158.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_6158.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_6158.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_6158.R'

effects_0500_200_6158.RData: effects_0500_200_6158.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_6158.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_6158.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_6158.R'

effects_1000_025_6158.RData: effects_1000_025_6158.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_6158.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_6158.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_6158.R'

effects_0500_025_6158.RData: effects_0500_025_6158.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_6158.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_6158.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_6158.R'

effects_1000_200_6158.RData: effects_1000_200_6158.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_6158.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_6158.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_6158.R'

effects_1000_150_6158.RData: effects_1000_150_6158.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_6158.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_6158.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_6158.R'

effects_1000_100_6158.RData: effects_1000_100_6158.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_6158.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_6158.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_6158.R'

effects_0500_000_6158.RData: effects_0500_000_6158.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_6158.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_6158.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_6158.R'

effects_1000_050_6158.RData: effects_1000_050_6158.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_6158.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_6158.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_6158.R'

effects_0500_050_6158.RData: effects_0500_050_6158.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_6158.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_6158.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_6158.R'

effects_0500_100_6181.RData: effects_0500_100_6181.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_6181.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_6181.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_6181.R'

effects_1000_025_6181.RData: effects_1000_025_6181.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_6181.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_6181.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_6181.R'

effects_0500_000_6181.RData: effects_0500_000_6181.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_6181.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_6181.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_6181.R'

effects_1000_200_6181.RData: effects_1000_200_6181.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_6181.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_6181.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_6181.R'

effects_0500_050_6181.RData: effects_0500_050_6181.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_6181.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_6181.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_6181.R'

effects_0500_150_6181.RData: effects_0500_150_6181.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_6181.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_6181.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_6181.R'

effects_0500_025_6181.RData: effects_0500_025_6181.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_6181.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_6181.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_6181.R'

effects_1000_150_6181.RData: effects_1000_150_6181.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_6181.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_6181.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_6181.R'

effects_0500_200_6181.RData: effects_0500_200_6181.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_6181.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_6181.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_6181.R'

effects_1000_000_6181.RData: effects_1000_000_6181.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_6181.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_6181.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_6181.R'

effects_1000_050_6181.RData: effects_1000_050_6181.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_6181.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_6181.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_6181.R'

effects_1000_100_6181.RData: effects_1000_100_6181.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_6181.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_6181.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_6181.R'

effects_0500_200_6207.RData: effects_0500_200_6207.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_6207.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_6207.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_6207.R'

effects_0500_150_6207.RData: effects_0500_150_6207.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_6207.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_6207.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_6207.R'

effects_1000_150_6207.RData: effects_1000_150_6207.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_6207.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_6207.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_6207.R'

effects_1000_050_6207.RData: effects_1000_050_6207.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_6207.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_6207.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_6207.R'

effects_0500_050_6207.RData: effects_0500_050_6207.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_6207.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_6207.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_6207.R'

effects_1000_100_6207.RData: effects_1000_100_6207.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_6207.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_6207.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_6207.R'

effects_0500_000_6207.RData: effects_0500_000_6207.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_6207.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_6207.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_6207.R'

effects_1000_025_6207.RData: effects_1000_025_6207.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_6207.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_6207.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_6207.R'

effects_0500_100_6207.RData: effects_0500_100_6207.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_6207.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_6207.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_6207.R'

effects_1000_200_6207.RData: effects_1000_200_6207.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_6207.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_6207.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_6207.R'

effects_1000_000_6207.RData: effects_1000_000_6207.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_6207.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_6207.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_6207.R'

effects_0500_025_6207.RData: effects_0500_025_6207.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_6207.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_6207.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_6207.R'

effects_0500_150_6574.RData: effects_0500_150_6574.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_6574.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_6574.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_6574.R'

effects_0500_000_6574.RData: effects_0500_000_6574.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_6574.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_6574.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_6574.R'

effects_1000_100_6574.RData: effects_1000_100_6574.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_6574.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_6574.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_6574.R'

effects_0500_050_6574.RData: effects_0500_050_6574.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_6574.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_6574.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_6574.R'

effects_0500_100_6574.RData: effects_0500_100_6574.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_6574.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_6574.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_6574.R'

effects_0500_025_6574.RData: effects_0500_025_6574.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_6574.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_6574.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_6574.R'

effects_1000_025_6574.RData: effects_1000_025_6574.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_6574.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_6574.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_6574.R'

effects_1000_000_6574.RData: effects_1000_000_6574.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_6574.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_6574.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_6574.R'

effects_1000_200_6574.RData: effects_1000_200_6574.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_6574.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_6574.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_6574.R'

effects_1000_050_6574.RData: effects_1000_050_6574.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_6574.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_6574.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_6574.R'

effects_1000_150_6574.RData: effects_1000_150_6574.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_6574.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_6574.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_6574.R'

effects_0500_200_6574.RData: effects_0500_200_6574.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_6574.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_6574.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_6574.R'

effects_0500_150_6581.RData: effects_0500_150_6581.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_6581.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_6581.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_6581.R'

effects_0500_100_6581.RData: effects_0500_100_6581.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_6581.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_6581.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_6581.R'

effects_0500_000_6581.RData: effects_0500_000_6581.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_6581.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_6581.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_6581.R'

effects_1000_000_6581.RData: effects_1000_000_6581.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_6581.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_6581.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_6581.R'

effects_1000_050_6581.RData: effects_1000_050_6581.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_6581.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_6581.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_6581.R'

effects_0500_200_6581.RData: effects_0500_200_6581.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_6581.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_6581.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_6581.R'

effects_1000_100_6581.RData: effects_1000_100_6581.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_6581.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_6581.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_6581.R'

effects_1000_025_6581.RData: effects_1000_025_6581.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_6581.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_6581.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_6581.R'

effects_1000_200_6581.RData: effects_1000_200_6581.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_6581.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_6581.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_6581.R'

effects_0500_050_6581.RData: effects_0500_050_6581.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_6581.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_6581.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_6581.R'

effects_1000_150_6581.RData: effects_1000_150_6581.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_6581.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_6581.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_6581.R'

effects_0500_025_6581.RData: effects_0500_025_6581.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_6581.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_6581.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_6581.R'

effects_0500_150_6742.RData: effects_0500_150_6742.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_6742.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_6742.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_6742.R'

effects_1000_025_6742.RData: effects_1000_025_6742.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_6742.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_6742.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_6742.R'

effects_1000_200_6742.RData: effects_1000_200_6742.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_6742.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_6742.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_6742.R'

effects_0500_100_6742.RData: effects_0500_100_6742.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_6742.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_6742.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_6742.R'

effects_0500_025_6742.RData: effects_0500_025_6742.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_6742.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_6742.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_6742.R'

effects_1000_100_6742.RData: effects_1000_100_6742.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_6742.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_6742.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_6742.R'

effects_1000_000_6742.RData: effects_1000_000_6742.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_6742.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_6742.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_6742.R'

effects_0500_000_6742.RData: effects_0500_000_6742.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_6742.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_6742.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_6742.R'

effects_0500_200_6742.RData: effects_0500_200_6742.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_6742.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_6742.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_6742.R'

effects_1000_050_6742.RData: effects_1000_050_6742.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_6742.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_6742.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_6742.R'

effects_1000_150_6742.RData: effects_1000_150_6742.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_6742.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_6742.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_6742.R'

effects_0500_050_6742.RData: effects_0500_050_6742.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_6742.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_6742.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_6742.R'

effects_0500_150_7442.RData: effects_0500_150_7442.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_7442.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_7442.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_7442.R'

effects_0500_050_7442.RData: effects_0500_050_7442.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_7442.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_7442.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_7442.R'

effects_1000_200_7442.RData: effects_1000_200_7442.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_7442.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_7442.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_7442.R'

effects_1000_000_7442.RData: effects_1000_000_7442.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_7442.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_7442.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_7442.R'

effects_1000_050_7442.RData: effects_1000_050_7442.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_7442.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_7442.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_7442.R'

effects_0500_000_7442.RData: effects_0500_000_7442.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_7442.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_7442.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_7442.R'

effects_1000_025_7442.RData: effects_1000_025_7442.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_7442.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_7442.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_7442.R'

effects_1000_100_7442.RData: effects_1000_100_7442.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_7442.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_7442.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_7442.R'

effects_0500_100_7442.RData: effects_0500_100_7442.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_7442.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_7442.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_7442.R'

effects_0500_200_7442.RData: effects_0500_200_7442.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_7442.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_7442.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_7442.R'

effects_1000_150_7442.RData: effects_1000_150_7442.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_7442.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_7442.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_7442.R'

effects_0500_025_7442.RData: effects_0500_025_7442.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_7442.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_7442.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_7442.R'

effects_0500_100_7758.RData: effects_0500_100_7758.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_7758.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_7758.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_7758.R'

effects_1000_200_7758.RData: effects_1000_200_7758.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_7758.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_7758.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_7758.R'

effects_1000_000_7758.RData: effects_1000_000_7758.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_7758.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_7758.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_7758.R'

effects_0500_150_7758.RData: effects_0500_150_7758.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_7758.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_7758.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_7758.R'

effects_1000_150_7758.RData: effects_1000_150_7758.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_7758.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_7758.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_7758.R'

effects_0500_050_7758.RData: effects_0500_050_7758.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_7758.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_7758.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_7758.R'

effects_0500_025_7758.RData: effects_0500_025_7758.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_7758.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_7758.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_7758.R'

effects_0500_200_7758.RData: effects_0500_200_7758.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_7758.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_7758.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_7758.R'

effects_1000_050_7758.RData: effects_1000_050_7758.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_7758.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_7758.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_7758.R'

effects_1000_025_7758.RData: effects_1000_025_7758.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_7758.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_7758.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_7758.R'

effects_0500_000_7758.RData: effects_0500_000_7758.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_7758.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_7758.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_7758.R'

effects_1000_100_7758.RData: effects_1000_100_7758.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_7758.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_7758.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_7758.R'

effects_1000_150_7768.RData: effects_1000_150_7768.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_7768.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_7768.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_7768.R'

effects_1000_000_7768.RData: effects_1000_000_7768.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_7768.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_7768.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_7768.R'

effects_0500_025_7768.RData: effects_0500_025_7768.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_7768.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_7768.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_7768.R'

effects_1000_100_7768.RData: effects_1000_100_7768.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_7768.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_7768.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_7768.R'

effects_0500_050_7768.RData: effects_0500_050_7768.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_7768.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_7768.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_7768.R'

effects_0500_150_7768.RData: effects_0500_150_7768.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_7768.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_7768.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_7768.R'

effects_0500_200_7768.RData: effects_0500_200_7768.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_7768.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_7768.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_7768.R'

effects_1000_050_7768.RData: effects_1000_050_7768.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_7768.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_7768.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_7768.R'

effects_0500_000_7768.RData: effects_0500_000_7768.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_7768.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_7768.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_7768.R'

effects_0500_100_7768.RData: effects_0500_100_7768.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_7768.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_7768.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_7768.R'

effects_1000_025_7768.RData: effects_1000_025_7768.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_7768.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_7768.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_7768.R'

effects_1000_200_7768.RData: effects_1000_200_7768.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_7768.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_7768.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_7768.R'

effects_1000_150_8245.RData: effects_1000_150_8245.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_8245.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_8245.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_8245.R'

effects_1000_100_8245.RData: effects_1000_100_8245.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_8245.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_8245.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_8245.R'

effects_0500_025_8245.RData: effects_0500_025_8245.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_8245.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_8245.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_8245.R'

effects_1000_025_8245.RData: effects_1000_025_8245.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_8245.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_8245.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_8245.R'

effects_1000_000_8245.RData: effects_1000_000_8245.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_8245.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_8245.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_8245.R'

effects_0500_000_8245.RData: effects_0500_000_8245.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_8245.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_8245.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_8245.R'

effects_0500_150_8245.RData: effects_0500_150_8245.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_8245.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_8245.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_8245.R'

effects_0500_200_8245.RData: effects_0500_200_8245.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_8245.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_8245.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_8245.R'

effects_1000_050_8245.RData: effects_1000_050_8245.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_8245.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_8245.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_8245.R'

effects_1000_200_8245.RData: effects_1000_200_8245.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_8245.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_8245.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_8245.R'

effects_0500_100_8245.RData: effects_0500_100_8245.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_8245.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_8245.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_8245.R'

effects_0500_050_8245.RData: effects_0500_050_8245.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_8245.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_8245.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_8245.R'

effects_0500_000_8264.RData: effects_0500_000_8264.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_8264.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_8264.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_8264.R'

effects_1000_050_8264.RData: effects_1000_050_8264.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_8264.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_8264.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_8264.R'

effects_0500_150_8264.RData: effects_0500_150_8264.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_8264.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_8264.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_8264.R'

effects_1000_000_8264.RData: effects_1000_000_8264.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_8264.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_8264.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_8264.R'

effects_0500_025_8264.RData: effects_0500_025_8264.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_8264.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_8264.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_8264.R'

effects_1000_150_8264.RData: effects_1000_150_8264.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_8264.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_8264.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_8264.R'

effects_0500_200_8264.RData: effects_0500_200_8264.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_8264.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_8264.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_8264.R'

effects_0500_050_8264.RData: effects_0500_050_8264.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_8264.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_8264.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_8264.R'

effects_1000_025_8264.RData: effects_1000_025_8264.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_8264.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_8264.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_8264.R'

effects_1000_200_8264.RData: effects_1000_200_8264.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_8264.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_8264.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_8264.R'

effects_1000_100_8264.RData: effects_1000_100_8264.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_8264.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_8264.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_8264.R'

effects_0500_100_8264.RData: effects_0500_100_8264.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_8264.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_8264.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_8264.R'

effects_1000_100_8424.RData: effects_1000_100_8424.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_8424.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_8424.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_8424.R'

effects_1000_150_8424.RData: effects_1000_150_8424.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_8424.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_8424.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_8424.R'

effects_0500_025_8424.RData: effects_0500_025_8424.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_8424.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_8424.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_8424.R'

effects_1000_025_8424.RData: effects_1000_025_8424.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_8424.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_8424.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_8424.R'

effects_1000_050_8424.RData: effects_1000_050_8424.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_8424.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_8424.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_8424.R'

effects_0500_050_8424.RData: effects_0500_050_8424.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_8424.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_8424.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_8424.R'

effects_0500_100_8424.RData: effects_0500_100_8424.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_8424.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_8424.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_8424.R'

effects_0500_200_8424.RData: effects_0500_200_8424.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_8424.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_8424.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_8424.R'

effects_1000_000_8424.RData: effects_1000_000_8424.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_8424.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_8424.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_8424.R'

effects_0500_000_8424.RData: effects_0500_000_8424.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_8424.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_8424.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_8424.R'

effects_0500_150_8424.RData: effects_0500_150_8424.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_8424.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_8424.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_8424.R'

effects_1000_200_8424.RData: effects_1000_200_8424.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_8424.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_8424.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_8424.R'

effects_0500_150_8553.RData: effects_0500_150_8553.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_8553.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_8553.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_8553.R'

effects_0500_050_8553.RData: effects_0500_050_8553.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_8553.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_8553.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_8553.R'

effects_1000_025_8553.RData: effects_1000_025_8553.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_8553.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_8553.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_8553.R'

effects_0500_200_8553.RData: effects_0500_200_8553.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_8553.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_8553.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_8553.R'

effects_1000_100_8553.RData: effects_1000_100_8553.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_8553.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_8553.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_8553.R'

effects_0500_000_8553.RData: effects_0500_000_8553.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_8553.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_8553.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_8553.R'

effects_1000_200_8553.RData: effects_1000_200_8553.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_8553.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_8553.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_8553.R'

effects_1000_000_8553.RData: effects_1000_000_8553.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_8553.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_8553.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_8553.R'

effects_1000_150_8553.RData: effects_1000_150_8553.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_8553.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_8553.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_8553.R'

effects_1000_050_8553.RData: effects_1000_050_8553.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_8553.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_8553.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_8553.R'

effects_0500_100_8553.RData: effects_0500_100_8553.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_8553.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_8553.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_8553.R'

effects_0500_025_8553.RData: effects_0500_025_8553.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_8553.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_8553.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_8553.R'

effects_0500_150_8714.RData: effects_0500_150_8714.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_8714.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_8714.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_8714.R'

effects_1000_000_8714.RData: effects_1000_000_8714.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_8714.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_8714.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_8714.R'

effects_1000_025_8714.RData: effects_1000_025_8714.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_8714.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_8714.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_8714.R'

effects_1000_050_8714.RData: effects_1000_050_8714.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_8714.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_8714.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_8714.R'

effects_0500_025_8714.RData: effects_0500_025_8714.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_8714.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_8714.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_8714.R'

effects_1000_150_8714.RData: effects_1000_150_8714.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_8714.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_8714.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_8714.R'

effects_0500_100_8714.RData: effects_0500_100_8714.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_8714.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_8714.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_8714.R'

effects_1000_200_8714.RData: effects_1000_200_8714.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_8714.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_8714.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_8714.R'

effects_0500_050_8714.RData: effects_0500_050_8714.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_8714.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_8714.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_8714.R'

effects_0500_000_8714.RData: effects_0500_000_8714.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_8714.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_8714.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_8714.R'

effects_0500_200_8714.RData: effects_0500_200_8714.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_8714.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_8714.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_8714.R'

effects_1000_100_8714.RData: effects_1000_100_8714.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_8714.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_8714.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_8714.R'

effects_1000_050_8841.RData: effects_1000_050_8841.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_8841.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_8841.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_8841.R'

effects_0500_200_8841.RData: effects_0500_200_8841.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_8841.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_8841.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_8841.R'

effects_0500_025_8841.RData: effects_0500_025_8841.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_8841.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_8841.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_8841.R'

effects_1000_000_8841.RData: effects_1000_000_8841.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_8841.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_8841.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_8841.R'

effects_0500_000_8841.RData: effects_0500_000_8841.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_8841.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_8841.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_8841.R'

effects_0500_050_8841.RData: effects_0500_050_8841.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_8841.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_8841.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_8841.R'

effects_1000_200_8841.RData: effects_1000_200_8841.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_8841.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_8841.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_8841.R'

effects_1000_025_8841.RData: effects_1000_025_8841.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_8841.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_8841.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_8841.R'

effects_1000_100_8841.RData: effects_1000_100_8841.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_8841.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_8841.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_8841.R'

effects_0500_150_8841.RData: effects_0500_150_8841.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_8841.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_8841.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_8841.R'

effects_1000_150_8841.RData: effects_1000_150_8841.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_8841.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_8841.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_8841.R'

effects_0500_100_8841.RData: effects_0500_100_8841.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_8841.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_8841.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_8841.R'

effects_0500_150_8921.RData: effects_0500_150_8921.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_8921.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_8921.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_8921.R'

effects_1000_025_8921.RData: effects_1000_025_8921.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_8921.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_8921.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_8921.R'

effects_1000_000_8921.RData: effects_1000_000_8921.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_8921.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_8921.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_8921.R'

effects_0500_000_8921.RData: effects_0500_000_8921.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_8921.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_8921.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_8921.R'

effects_0500_050_8921.RData: effects_0500_050_8921.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_8921.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_8921.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_8921.R'

effects_1000_050_8921.RData: effects_1000_050_8921.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_8921.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_8921.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_8921.R'

effects_1000_100_8921.RData: effects_1000_100_8921.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_8921.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_8921.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_8921.R'

effects_0500_200_8921.RData: effects_0500_200_8921.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_8921.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_8921.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_8921.R'

effects_0500_100_8921.RData: effects_0500_100_8921.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_8921.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_8921.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_8921.R'

effects_1000_150_8921.RData: effects_1000_150_8921.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_8921.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_8921.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_8921.R'

effects_0500_025_8921.RData: effects_0500_025_8921.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_8921.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_8921.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_8921.R'

effects_1000_200_8921.RData: effects_1000_200_8921.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_8921.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_8921.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_8921.R'

effects_0500_050_9132.RData: effects_0500_050_9132.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_9132.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_9132.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_9132.R'

effects_1000_100_9132.RData: effects_1000_100_9132.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_9132.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_9132.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_9132.R'

effects_1000_150_9132.RData: effects_1000_150_9132.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_9132.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_9132.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_9132.R'

effects_1000_000_9132.RData: effects_1000_000_9132.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_9132.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_9132.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_9132.R'

effects_1000_050_9132.RData: effects_1000_050_9132.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_9132.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_9132.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_9132.R'

effects_1000_200_9132.RData: effects_1000_200_9132.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_9132.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_9132.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_9132.R'

effects_0500_200_9132.RData: effects_0500_200_9132.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_9132.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_9132.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_9132.R'

effects_0500_025_9132.RData: effects_0500_025_9132.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_9132.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_9132.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_9132.R'

effects_0500_000_9132.RData: effects_0500_000_9132.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_9132.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_9132.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_9132.R'

effects_1000_025_9132.RData: effects_1000_025_9132.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_9132.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_9132.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_9132.R'

effects_0500_150_9132.RData: effects_0500_150_9132.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_9132.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_9132.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_9132.R'

effects_0500_100_9132.RData: effects_0500_100_9132.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_9132.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_9132.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_9132.R'

effects_1000_200_9189.RData: effects_1000_200_9189.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_9189.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_9189.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_9189.R'

effects_0500_100_9189.RData: effects_0500_100_9189.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_9189.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_9189.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_9189.R'

effects_0500_000_9189.RData: effects_0500_000_9189.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_9189.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_9189.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_9189.R'

effects_0500_200_9189.RData: effects_0500_200_9189.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_9189.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_9189.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_9189.R'

effects_1000_100_9189.RData: effects_1000_100_9189.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_9189.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_9189.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_9189.R'

effects_1000_025_9189.RData: effects_1000_025_9189.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_9189.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_9189.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_9189.R'

effects_0500_150_9189.RData: effects_0500_150_9189.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_9189.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_9189.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_9189.R'

effects_0500_025_9189.RData: effects_0500_025_9189.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_9189.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_9189.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_9189.R'

effects_0500_050_9189.RData: effects_0500_050_9189.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_9189.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_9189.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_9189.R'

effects_1000_000_9189.RData: effects_1000_000_9189.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_9189.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_9189.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_9189.R'

effects_1000_050_9189.RData: effects_1000_050_9189.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_9189.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_9189.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_9189.R'

effects_1000_150_9189.RData: effects_1000_150_9189.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_9189.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_9189.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_9189.R'

effects_1000_000_9459.RData: effects_1000_000_9459.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_9459.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_9459.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_9459.R'

effects_1000_025_9459.RData: effects_1000_025_9459.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_9459.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_9459.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_9459.R'

effects_1000_050_9459.RData: effects_1000_050_9459.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_9459.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_9459.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_9459.R'

effects_0500_150_9459.RData: effects_0500_150_9459.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_9459.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_9459.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_9459.R'

effects_0500_200_9459.RData: effects_0500_200_9459.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_9459.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_9459.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_9459.R'

effects_0500_050_9459.RData: effects_0500_050_9459.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_9459.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_9459.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_9459.R'

effects_1000_150_9459.RData: effects_1000_150_9459.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_9459.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_9459.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_9459.R'

effects_0500_100_9459.RData: effects_0500_100_9459.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_9459.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_9459.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_9459.R'

effects_1000_200_9459.RData: effects_1000_200_9459.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_9459.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_9459.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_9459.R'

effects_0500_025_9459.RData: effects_0500_025_9459.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_9459.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_9459.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_9459.R'

effects_1000_100_9459.RData: effects_1000_100_9459.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_9459.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_9459.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_9459.R'

effects_0500_000_9459.RData: effects_0500_000_9459.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_9459.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_9459.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_9459.R'

effects_1000_050_9697.RData: effects_1000_050_9697.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_9697.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_9697.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_9697.R'

effects_0500_050_9697.RData: effects_0500_050_9697.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_9697.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_9697.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_9697.R'

effects_1000_100_9697.RData: effects_1000_100_9697.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_9697.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_9697.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_9697.R'

effects_0500_100_9697.RData: effects_0500_100_9697.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_9697.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_9697.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_9697.R'

effects_1000_000_9697.RData: effects_1000_000_9697.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_9697.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_9697.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_9697.R'

effects_0500_200_9697.RData: effects_0500_200_9697.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_9697.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_9697.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_9697.R'

effects_0500_150_9697.RData: effects_0500_150_9697.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_9697.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_9697.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_9697.R'

effects_1000_150_9697.RData: effects_1000_150_9697.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_9697.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_9697.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_9697.R'

effects_1000_200_9697.RData: effects_1000_200_9697.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_9697.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_9697.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_9697.R'

effects_0500_000_9697.RData: effects_0500_000_9697.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_9697.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_9697.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_9697.R'

effects_1000_025_9697.RData: effects_1000_025_9697.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_9697.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_9697.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_9697.R'

effects_0500_025_9697.RData: effects_0500_025_9697.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_9697.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_9697.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_9697.R'

effects_1000_050_9795.RData: effects_1000_050_9795.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_9795.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_9795.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_9795.R'

effects_0500_100_9795.RData: effects_0500_100_9795.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_9795.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_9795.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_9795.R'

effects_1000_000_9795.RData: effects_1000_000_9795.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_9795.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_9795.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_9795.R'

effects_0500_150_9795.RData: effects_0500_150_9795.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_9795.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_9795.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_9795.R'

effects_0500_050_9795.RData: effects_0500_050_9795.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_9795.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_9795.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_9795.R'

effects_1000_200_9795.RData: effects_1000_200_9795.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_9795.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_9795.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_9795.R'

effects_1000_150_9795.RData: effects_1000_150_9795.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_9795.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_9795.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_9795.R'

effects_1000_025_9795.RData: effects_1000_025_9795.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_9795.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_9795.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_9795.R'

effects_1000_100_9795.RData: effects_1000_100_9795.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_9795.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_9795.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_9795.R'

effects_0500_000_9795.RData: effects_0500_000_9795.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_9795.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_9795.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_9795.R'

effects_0500_200_9795.RData: effects_0500_200_9795.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_9795.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_9795.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_9795.R'

effects_0500_025_9795.RData: effects_0500_025_9795.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_9795.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_9795.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_9795.R'

effects_1000_000_9968.RData: effects_1000_000_9968.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_000_9968.R'
	 @$(RCMD) BATCH --vanilla effects_1000_000_9968.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_000_9968.R'

effects_0500_200_9968.RData: effects_0500_200_9968.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_200_9968.R'
	 @$(RCMD) BATCH --vanilla effects_0500_200_9968.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_200_9968.R'

effects_0500_000_9968.RData: effects_0500_000_9968.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_000_9968.R'
	 @$(RCMD) BATCH --vanilla effects_0500_000_9968.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_000_9968.R'

effects_0500_150_9968.RData: effects_0500_150_9968.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_150_9968.R'
	 @$(RCMD) BATCH --vanilla effects_0500_150_9968.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_150_9968.R'

effects_1000_050_9968.RData: effects_1000_050_9968.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_050_9968.R'
	 @$(RCMD) BATCH --vanilla effects_1000_050_9968.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_050_9968.R'

effects_1000_150_9968.RData: effects_1000_150_9968.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_150_9968.R'
	 @$(RCMD) BATCH --vanilla effects_1000_150_9968.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_150_9968.R'

effects_1000_100_9968.RData: effects_1000_100_9968.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_100_9968.R'
	 @$(RCMD) BATCH --vanilla effects_1000_100_9968.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_100_9968.R'

effects_0500_050_9968.RData: effects_0500_050_9968.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_050_9968.R'
	 @$(RCMD) BATCH --vanilla effects_0500_050_9968.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_050_9968.R'

effects_0500_025_9968.RData: effects_0500_025_9968.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_025_9968.R'
	 @$(RCMD) BATCH --vanilla effects_0500_025_9968.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_025_9968.R'

effects_0500_100_9968.RData: effects_0500_100_9968.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_0500_100_9968.R'
	 @$(RCMD) BATCH --vanilla effects_0500_100_9968.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_0500_100_9968.R'

effects_1000_200_9968.RData: effects_1000_200_9968.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_200_9968.R'
	 @$(RCMD) BATCH --vanilla effects_1000_200_9968.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_200_9968.R'

effects_1000_025_9968.RData: effects_1000_025_9968.R 
	 @date '+ %Y-%m-%d %H:%M:%S: + effects_1000_025_9968.R'
	 @$(RCMD) BATCH --vanilla effects_1000_025_9968.R
	 @date '+ %Y-%m-%d %H:%M:%S: - effects_1000_025_9968.R'

