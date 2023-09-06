#! /usr/bin/env python3

# SPDX-License-Identifier: BSD-2-Clause
#
# Copyright (c) 2021-2023 Alexandre Joannou
# Copyright (c) 2023 Peter Rugg
# All rights reserved.
#
# This material is based upon work supported by the DoD Information Analysis
# Center Program Management Office (DoD IAC PMO), sponsored by the Defense
# Technical Information Center (DTIC) under Contract No. FA807518D0004.  Any
# opinions, findings and conclusions or recommendations expressed in this
# material are those of the author(s) and do not necessarily reflect the views
# of the Air Force Installation Contracting Agency (AFICA).
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
# OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
# SUCH DAMAGE.
#

import subprocess
import argparse
import pexpect
import time
import sys
import abc
import re
import os

####################
# helper functions #
################################################################################

######## a 'which' helper function
######## (from https://stackoverflow.com/a/377028)
def which(prg):
  def is_exe(fpath):
    return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

  fpath, fname = os.path.split(prg)
  if fpath:
    if is_exe(prg):
      return prg
  else:
    for path in os.environ.get('PATH').split(os.pathsep):
      exe_file = os.path.join(path, prg)
      if is_exe(exe_file):
        return exe_file
  return None

def get_exec(prg):
  p = which(prg)
  if p == None:
    print("'" + prg + "' executable not in path")
    exit(-1)
  return p

def get_file(fname):
  f = os.path.realpath(fname)
  if not os.path.exists(f):
    print("'" + f + "' does not exist")
    exit(-1)
  return f

## peter's stuff
## from https://stackoverflow.com/a/59413525
################################################################################
################################################################################

class FilterExpecter(pexpect.Expecter):
  magicToks = ["\x1b[?25h"]
  def __init__(self, *args, **kwargs):
    super().__init__(*args, **kwargs)
    self.pending = FilterExpecter.magicToks

  def new_data(self, data):
    ret = ""
    for c in data.decode():
      self.pending = [p for p in self.pending if p[0] == c]
      if not self.pending:
        ret += c
      self.pending = [ p[1:] for p in self.pending
                             if len(p) > 1 ] + FilterExpecter.magicToks
    return super().new_data(ret.encode())

class FilterExpectSpawn(pexpect.spawn):
  def expect_list(self, pats, timeout=-1, searchwindowsize=-1, unsync=False):
    if timeout == -1:
      timeout = self.timeout
    exp = FilterExpecter(self, pexpect.expect.searcher_re(pats)
                             , searchwindowsize)
    return exp.expect_loop(timeout)

#######################################
# DE10Pro session configuration class #
################################################################################

class DE10ProSession:
  """A DE10Pro session context"""

  def __init__(self, hps_rbf, core_rbf
                   , arm_use_block_device, arm_bsd_loader, arm_bsd_kernel
                   , to_step
                   , vlvl):
    ######## quartus conf.
    # fail if incomplete config.
    self.quartus_pgm = get_exec('quartus_pgm')
    self.jtagconfig = get_exec('jtagconfig')
    ######## rbf conf.
    self.core_rbf = core_rbf
    self.hps_rbf = get_file(hps_rbf)
    ######## serial terminal conf.
    self.serial_tty = get_file(os.path.join('/dev', 'ttyACM0'))
    self.picocom = get_exec('picocom')
    self.minicom = get_exec('minicom')
    ######## arm system boot
    self.arm_use_block_device = arm_use_block_device
    self.arm_bsd_loader_addr = 0x02000000
    self.arm_bsd_loader = arm_bsd_loader
    self.arm_device_tree_addr = 0x08000000
    self.arm_device_tree = 'socfpga_stratix10_de10_pro2.dtb'
    self.arm_bsd_kernel = arm_bsd_kernel
    #
    self.encoding = None
    self.to_step = to_step
    self.vlvl = vlvl
    ######## pexpect handle
    if vlvl < 4:
      self.logfile = open('/tmp/de10pro-interact.log', 'wb')
    else:
      self.logfile = sys.stdout.buffer
    self.handle = None

  def vprint(self, lvl, msg):
    if self.vlvl >= lvl:
      print(msg)

class SessionStep(abc.ABC):
  @abc.abstractmethod
  def run(self):
    pass
  def __str__(self):
    return self.__class__.__name__.replace("Step_","")
  def __vprint(self, lvl, msg):
    self.session.vprint(lvl, msg)
  def call(self, session):
    self.session = session
    self.__vprint(1, f'{f" Starting step {self}":>>80}')
    try:
      self.run()
      self.__vprint(1, f'\n{f" Completed step {self}":>>80}')
    except pexpect.TIMEOUT as e:
      self.__vprint(0, f"TIMEOUT in step {self}")
      self.__vprint(1, str(e))
      self.__fallback(-1)
    if self.session.to_step == str(self):
      self.__fallback(0)
  def __fallback(self, retcode):
    if self.session.handle:
      self.__vprint(0, ">>>> falling back to interactive session <<<<")
      self.session.handle.interact()
      exit(retcode)
    else:
      print("no active pexpect session")
      exit(-1)

class Step_program_hps(SessionStep):
  def run(self):
    c = subprocess.Popen([self.session.jtagconfig], stdout=subprocess.PIPE)
    c.wait(timeout=10)
    fpga_dev_idx = 2
    for i, l in enumerate (c.stdout.readlines()):
      if "1SX280HH1" in l.decode():
        fpga_dev_idx = i
        break
    else:
      print("No 1SX280HH1 device was found")
      exit(-1)
    c = subprocess.Popen( [ self.session.quartus_pgm
                          , '-m', 'jtag'
                          , '-o', f'P;{self.session.hps_rbf}'
                                  f'@{fpga_dev_idx}' ]
                        , stdout=subprocess.DEVNULL
                        , stderr=subprocess.STDOUT )
    c.wait()

class Step_to_uboot(SessionStep):
  def run(self):
    ######## connect to serial tty
    picocom_args = [ '-q', '-b', '115200', self.session.serial_tty ]
    c = FilterExpectSpawn( self.session.picocom
                         , picocom_args
                         , encoding = self.session.encoding
                         , logfile = self.session.logfile )
    self.session.handle = c
    while True:
      try:
        c.expect('Hit any key to stop autoboot:')
        break
      except UnicodeDecodeError: # drain spurious characters
        continue
    c.sendline()
    c.expect('.* #')

class Step_load_core_rbf(SessionStep):
  def run(self):
    c = self.session.handle
    dev = self.session.arm_use_block_device
    if dev == 'usb':
      c.sendline('usb start')
      c.expect('.* #')
    c.sendline('fatload ' + dev + ' 0:1 1000 ' + self.session.core_rbf)
    c.expect('.* #')
    c.sendline('fpga load 0 1000 ${filesize}')
    c.expect('.* #')
    c.sendline('bridge enable')
    c.expect('.* #')

class Step_load_bsd_loader(SessionStep):
  def run(self):
    c = self.session.handle
    dev = self.session.arm_use_block_device
    c.sendline(f'fatload {dev} 0:1 '
               f'{hex(self.session.arm_bsd_loader_addr)} '
               f'{self.session.arm_bsd_loader}')
    c.expect('.* #')
    c.sendline(f'fatload {dev} 0:1 '
               f'{hex(self.session.arm_device_tree_addr)} '
               f'{self.session.arm_device_tree}')
    c.expect('.* #')

class Step_boot_bsd_loader(SessionStep):
  def run(self):
    c = self.session.handle
    c.sendline(f'bootefi'
               f' {hex(self.session.arm_bsd_loader_addr)} '
               f' {hex(self.session.arm_device_tree_addr)}')
    time.sleep(5)
    #c.expect('OK ')

class Step_bsd_loader_boot_kernel(SessionStep):
  def run(self):
    c = self.session.handle
    fatdev = 'disk0s1:'
    ufsdev = 'disk0s2:'
    if self.session.arm_use_block_device == 'usb':
      fatdev = 'disk1s1:'
      ufsdev = 'disk1s2:'
    c.sendline('load ' + fatdev + self.session.arm_bsd_kernel)
    c.expect('OK ')
    c.sendline('set currdev=' + ufsdev)
    c.expect('OK ')
    c.sendline('include /boot/lua/loader.lua')
    c.expect(' Boot Options')
    c.sendline('\r')
    c.expect_exact('login:', timeout = 60)
    c.sendline('root')
    c.expect_exact('root@')

class Step_boot_riscv_core(SessionStep):
  def run(self):
    c = self.session.handle
    c.sendline('/root/riscv-freebsd-boot.sh')
    c.expect_exact('RISCV core ready', timeout = 300)
    c.sendline('cu -l /dev/ttyu0')
    c.sendline('\r')
    c.expect_exact('exec /bin/sh', timeout = 60)
    c.expect_exact('#')
steps = [ Step_program_hps()
        , Step_to_uboot()
        , Step_load_core_rbf()
        , Step_load_bsd_loader()
        , Step_boot_bsd_loader()
        , Step_bsd_loader_boot_kernel()
        , Step_boot_riscv_core() ]

################################
# Parse command line arguments #
################################################################################

def auto_int(x):
  return int(x, 0)

def auto_pos_int(x):
  val = int(x, 0)
  if val <= 0:
    raise argparse.ArgumentTypeError("argument must be a positive int. Got {:d}.".format(val))
  return val

parser = argparse.ArgumentParser(description='Runs a DE10Pro session')

parser.add_argument( '--hps-rbf', metavar='HPS_RBF'
                   , type=str, default='socfpga.hps.rbf'
                   , help="The rbf slice for initial ARM stratix10 conf. to be found on the host machine.")

parser.add_argument( '--core-rbf', metavar='CORE_RBF'
                   , type=str, default='socfpga.core.rbf'
                   , help="The name of the rbf slice for fpga stratix10 conf. to be found on the boot sdcard / usb device.")

parser.add_argument( '--use-block-device', metavar='BLOCK_DEVICE'
                   , type=str, choices=['mmc', 'usb'], default='mmc'
                   , help="The block device to use to search for files (mmc(default) or usb)")

parser.add_argument( '--arm-bsd-loader', metavar='ARM_BSD_LOADER'
                   , type=str, default='efi/boot/bootaa64.efi'
                   , help="The name of the arm bsd loader to run on the hps arm core. to be found on the boot sdcard / usb device.")

parser.add_argument( '--arm-bsd-kernel', metavar='ARM_BSD_KERNEL'
                   , type=str, default='de10-kernel-fmem'
                   , help="The name of the arm bsd kernel to run on the hps arm core. to be found on the boot sdcard / usb device.")

parser.add_argument( '-v', '--verbosity', metavar='VERB', type=auto_int
                   , default=1, help="set verbosity level")

parser.add_argument( '--to-step', metavar='STEP'
                   , type=str, default=str(steps[-1]), choices=[str(s) for s in steps]
                   , help="step to boot to, one of {:s}".format(str([str(s) for s in steps])))

################################################################################

if __name__ == "__main__":
  args = parser.parse_args()
  session = DE10ProSession( args.hps_rbf
                          , args.core_rbf
                          , args.use_block_device
                          , args.arm_bsd_loader
                          , args.arm_bsd_kernel
                          , args.to_step
                          , args.verbosity )
  for step in steps:
    step.call(session)
