#! /usr/bin/env sh

echo "build device tree for the de10 pro HPS"

WORKDIR=socfpga_stratix10_de10_pro.dtb_build
mkdir -p $WORKDIR
cd $WORKDIR

GITHUB_BASE=https://raw.githubusercontent.com
GITHUB_USER=terasic
GITHUB_REPO=linux-socfpga
#de10_pro_revD
#COMMIT_ISH=63f1e04694ec7bafc52005c96cc466dcfe6879ea
#de10_pro_revC
COMMIT_ISH=6143ea1943045351dd4a6d10d54a01906e8427cf
URL_BASE=$GITHUB_BASE/$GITHUB_USER/$GITHUB_REPO/$COMMIT_ISH
TOP="arch/arm64/boot/dts/altera/socfpga_stratix10_de10_pro.dts"
SRCS="
$TOP
arch/arm64/boot/dts/altera/socfpga_stratix10_de10_pro.dts
arch/arm64/boot/dts/altera/socfpga_stratix10.dtsi
include/dt-bindings/reset/altr,rst-mgr-s10.h
include/dt-bindings/gpio/gpio.h
include/dt-bindings/clock/stratix10-clock.h
"

echo "download sources from:"
echo "$GITHUB_BASE"
echo "$GITHUB_USER"
echo "$GITHUB_REPO"
echo "$COMMIT_ISH"

for f in $SRCS; do
  curl -o $f --create-dirs $URL_BASE/$f;
done

DTB=$(pwd)/$(basename $TOP .dts).dtb
cpp -B . -x assembler-with-cpp -P $TOP | dtc -I dts -O dtb -p 0x1000 -o $DTB \
&& echo "device tree binary built: $DTB"
