#!/bin/bash

docker build --platform linux/x86_64 -t egypt_wave3_screener .

docker tag egypt_wave3_screener saudiwin/egypt_wave3_screener:latest

docker push saudiwin/egypt_wave3_screener:latest

#aws ecr create-repository --repository-name tun_sen_wave3

# aws ecr get-login-password --region us-east-1 | docker login --username AWS --password-stdin 644037967733.dkr.ecr.us-east-1.amazonaws.com
#
# docker tag tun_sen_wave3:latest 644037967733.dkr.ecr.us-east-1.amazonaws.com/tun_sen_wave3:latest
#
# docker push 644037967733.dkr.ecr.us-east-1.amazonaws.com/tun_sen_wave3:latest
#
# aws ecr list-images --repository-name tun_sen_wave3






