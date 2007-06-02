package Device::Jtag::USB::FTCJTAG;

#use 5.008008;
use strict;
use warnings;

require Exporter;

our @ISA = qw(Exporter);

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration   use Device::Jtag::USB::FTCJTAG ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.
our %EXPORT_TAGS = ( 'all' => [ qw(

) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw(

);

our $VERSION = '0.05';


# Preloaded methods go here.

use Win32::API;

###################################################################################################
# Define FTC_STATUS return values
###################################################################################################
my $ftc_status_type_aref;
$ftc_status_type_aref->[ 0] = 'FTC_SUCCESS';
$ftc_status_type_aref->[ 1] = 'FTC_INVALID_HANDLE';
$ftc_status_type_aref->[ 2] = 'FTC_DEVICE_NOT_FOUND';
$ftc_status_type_aref->[ 3] = 'FTC_DEVICE_NOT_OPENED';
$ftc_status_type_aref->[ 4] = 'FTC_IO_ERROR';
$ftc_status_type_aref->[ 5] = 'FTC_INSUFFICIENT_RESOURCES';
$ftc_status_type_aref->[20] = 'FTC_FAILED_TO_COMPLETE_COMMAND';
$ftc_status_type_aref->[21] = 'FTC_FAILED_TO_SYCHRONIZE_DEVICE_MPSSE';
$ftc_status_type_aref->[22] = 'FTC_INVALID_DEVICE_NAME_INDEX';
$ftc_status_type_aref->[23] = 'FTC_NULL_DEVICE_NAME_BUFFER_POINTER';
$ftc_status_type_aref->[24] = 'FTC_DEVICE_NAME_BUFFER_TOO_SMALL';
$ftc_status_type_aref->[25] = 'FTC_INVALID_DEVICE_NAME';
$ftc_status_type_aref->[26] = 'FTC_INVALID_LOCATION_ID';
$ftc_status_type_aref->[27] = 'FTC_DEVICE_IN_USE';
$ftc_status_type_aref->[28] = 'FTC_TOO_MANY_DEVICES';
$ftc_status_type_aref->[29] = 'FTC_INVALID_FREQUENCY_VALUE';
$ftc_status_type_aref->[30] = 'FTC_NULL_INPUT_OUTPUT_BUFFER_POINTER';
$ftc_status_type_aref->[31] = 'FTC_INVALID_NUMBER_BITS';
$ftc_status_type_aref->[32] = 'FTC_NULL_WRITE_DATA_BUFFER_POINTER';
$ftc_status_type_aref->[33] = 'FTC_INVALID_NUMBER_BYTES';
$ftc_status_type_aref->[34] = 'FTC_NUMBER_BYTES_TOO_SMALL';
$ftc_status_type_aref->[35] = 'FTC_INVALID_TAP_CONTROLLER_STATE';
$ftc_status_type_aref->[36] = 'FTC_NULL_READ_DATA_BUFFER_POINTER';
$ftc_status_type_aref->[37] = 'FTC_NULL_DLL_VERSION_BUFFER_POINTER';
$ftc_status_type_aref->[38] = 'FTC_DLL_VERSION_BUFFER_TOO_SMALL';
$ftc_status_type_aref->[39] = 'FTC_NULL_LANGUAGE_CODE_BUFFER_POINTER';
$ftc_status_type_aref->[40] = 'FTC_NULL_ERROR_MESSAGE_BUFFER_POINTER';
$ftc_status_type_aref->[41] = 'FTC_ERROR_MESSAGE_BUFFER_TOO_SMALL';
$ftc_status_type_aref->[42] = 'FTC_INVALID_LANGUAGE_CODE';
$ftc_status_type_aref->[43] = 'FTC_INVALID_STATUS_CODE';

###################################################################################################
# Define JTAG TAP controller states
###################################################################################################
use constant TEST_LOGIC_STATE                 => 1;
use constant RUN_TEST_IDLE_STATE              => 2;
use constant PAUSE_TEST_DATA_REGISTER_STATE   => 3;
use constant PAUSE_INSTRUCTION_REGISTER_STATE => 4;
use constant SHIFT_TEST_DATA_REGISTER_STATE   => 5;
use constant SHIFT_INSTRUCTION_REGISTER_STATE => 6;

###################################################################################################
# Define FTDI chip buffer size (64k bits)
###################################################################################################
my $ftdi_buffer_size = 65535;

###################################################################################################
# Define Instruction Register vs Data Register constants
###################################################################################################
use constant IRSHIFT => 1;
use constant DRSHIFT => 0;

###################################################################################################
# Construct the new object
##################################################################################################
sub new {
  my $self = {};

  # Import API functions and assign to function handles
  foreach my $href (
    {perl_name => 'get_ndev', c_name => 'JTAG_GetNumDevices'      , inputs => 'P'     , outputs => 'N'},
    {perl_name => 'open'    , c_name => 'JTAG_Open'               , inputs => 'P'     , outputs => 'N'},
    {perl_name => 'init'    , c_name => 'JTAG_InitDevice'         , inputs => 'NN'    , outputs => 'N'},
    {perl_name => 'write'   , c_name => 'JTAG_Write'              , inputs => 'NNNPNN', outputs => 'N'},
    {perl_name => 'read'    , c_name => 'JTAG_Read'               , inputs => 'NNNPPN', outputs => 'N'},
    {perl_name => 'gen_clks', c_name => 'JTAG_GenerateClockPulses', inputs => 'NN'    , outputs => 'N'},
    {perl_name => 'get_gpio', c_name => 'JTAG_GetGPIOs'           , inputs => 'NNPNP' , outputs => 'N'},
    {perl_name => 'set_gpio', c_name => 'JTAG_SetGPIOs'           , inputs => 'NNPNP' , outputs => 'N'}) {

    $self->{fh}->{$href->{perl_name}} = Win32::API->new('FTCJTAG', $href->{c_name}, $href->{inputs}, $href->{outputs});
    if(not defined $self->{fh}->{$href->{perl_name}}) {
      die(sprintf("Unable to import API %s: %s\n", $href->{c_name}, $!));
    }
  }

  # Define JTAG scan chain device information
  $self->{di} = [
    {name   => 'XC3S1200E FPGA',         # device 0 name
     idregx => '^.1c2e093',              # device 0 idcode reg expression
     ircmds => {idcode => '001001',      # device 0 instructions
                user1  => '000010',
                user2  => '000011',
                bypass => '111111'}},

    {name   => 'XCF0XS Platform Flash',  # device 1 name
     idregx => '^.504.093',              # device 1 idcode reg expression
     ircmds => {idcode => '11111110',    # device 1 instructions
                bypass => '11111111'}}
  ];


  # Open JTAG key
  my $ftc_handle_lw = ' 'x4; # pre-allocate 4 bytes to long word
  my $ftc_status = $self->{fh}->{open}->Call($ftc_handle_lw);
  die(sprintf("ERROR : %s : %s\n", 'Unable to open JTAGkey', $ftc_status_type_aref->[$ftc_status])) if ($ftc_status);

  # Assign JTAGKey device handle
  $self->{key} = unpack('L', $ftc_handle_lw); # unpack long word

  # Initialize JTAGKey to 6MHz transfer rate
  $ftc_status = $self->{fh}->{init}->Call($self->{key},0);
  die(sprintf("ERROR : %s : %s\n", 'Unable to initialize JTAGkey', $ftc_status_type_aref->[$ftc_status])) if ($ftc_status);

  # Read JTAGKey GPIOs
  my $gpio_lo_lw   = ' 'x16; # pre-allocate 4x4 long words
  my $gpio_hi_lw   = ' 'x16; # pre-allocate 4x4 long words
  $ftc_status      = $self->{fh}->{get_gpio}->Call($self->{key}, 1, $gpio_lo_lw, 1, $gpio_hi_lw);
  die(sprintf("ERROR : %s : %s\n", 'Unable to read JTAGkey GPIOs', $ftc_status_type_aref->[$ftc_status])) if ($ftc_status);
  my $gpio_lo_aref = [unpack('L4', $gpio_lo_lw)]; # unpack 4 long words into array ref
  my $gpio_hi_aref = [unpack('L4', $gpio_hi_lw)]; # unpack 4 long words into array ref

  #foreach $b (0..3) {
  #  printf("GPIOL-%d = %d\n", $b, $gpio_lo_aref->[$b]);
  #}

  # Check JTAGKey VREF (GPIO LO bit 1 low if powered)
  die("ERROR : VREF not powered\n") if ($gpio_lo_aref->[1] eq 1);

  # Set JTAGKey GPIO controlling output enable
  my $gpiol_data = (0,0,0,0,0,0,0,1); # this does not make sense, but happens to work
  my $gpioh_data = (0,0,0,0,0,0,0,0);

  $ftc_status = $self->{fh}->{set_gpio}->Call($self->{key}, 1, pack('L8',$gpiol_data), 0, pack('L8',$gpioh_data));
  die(sprintf("ERROR : %s : %s\n", 'Unable to set JTAGkey GPIOs', $ftc_status_type_aref->[$ftc_status])) if ($ftc_status);

  # Check JTAGKey output enable (GPIO LO bit 0 low)
  #die("ERROR: JTAG_OE_N not driven low\n") if ($gpio_lo_aref->[0] eq 0);

  # Send JTAG devices to TEST LOGIC RESET state with a dummy write
  write_dev_ir($self, 0, 'idcode', TEST_LOGIC_STATE);

  print "JTAGKEY initialized\n";

  bless($self);
  return $self;
}

###########################################################################################################
# Instruction register write
# Usage: write_dev_ir($self, $deviceid, $instruction, [$endstate]);
# Return: hex string of data written to selected device
###########################################################################################################
sub write_dev_ir {
  my $self       = shift;
  my $seldev     = shift;                        # scan chain position of device to write, beginning with 0
  my $instruct   = shift;                        # instruction to write to selected device
  my $endstate   = shift;                        # state to leave device in


  # Find number of devices in scan chain
  my $nscdevs    = scalar(@{$self->{di}});

  # Get the binary string for the data to be written to selected device
  if (not defined $self->{di}->[$seldev]->{ircmds}->{$instruct}) {
    die "ERROR: Instruction $instruct not defined for scan chain device $seldev\n";
  }
  my $seldata = $self->{di}->[$seldev]->{ircmds}->{$instruct};
  my $data = '';

  # Construct full binary string by padding the non-selected device data with the BYPASS command
  foreach my $dev (0..$nscdevs-1) {
    if ($dev eq $seldev) {
      $data .= $seldata;
    } else {
      if (not defined $self->{di}->[$dev]->{ircmds}->{'bypass'}) {
        die "ERROR: Instruction bypass not defined for scan chain device $dev\n";
      }
      $data .= $self->{di}->[$dev]->{ircmds}->{'bypass'};
    }
  }

  # Write to device
  write_dev($self, $seldev, IRSHIFT, $data, $endstate);

  # Return data written to selected device as hex string
  return sprintf("%x", oct('0b'.$seldata));

}

###########################################################################################################
# Perform a data register write to JTAG scan chain device
# Usage: write_dev_dr($self, $deviceid, $data_binstring, [$endstate]);
# Return: hex string of data written to selected device
###########################################################################################################
sub write_dev_dr {
  my $self       = shift;
  my $seldev     = shift;                        # scan chain position, beginning with 0
  my $seldata    = shift;                        # expected as string of binary data
  my $endstate   = shift;                        # state to leave device in

  # Pad the data with 0's, depending upon the desired device's position in the scan chain
  # All devices in the scan chain -- other than the one that we're interested in -- are assumed to
  # be in the BYPASS state, representing a single flip flop in the chain.  So, if there are any devices
  # in the chain before the one we're writing to (this would be represented by the $seldev of the device
  # we're writing to being nonzero), then pad the end of the write string with a 0 for each device in
  # the chain preceeding the one we're writing to.
  my $data = $seldata . '0'x$seldev;

  # Write to device
  write_dev($self, $seldev, DRSHIFT, $data, $endstate);

  # Return data written to selected device as hex string
  return sprintf("%08x", oct('0b'.$seldata));
}

###########################################################################################################
# Write to JTAG device
# Usage: write_dev($self, $deviceid, $type (IRSHIFT or DRSHIFT), $data_binstring, [$endstate]);
# Return: hex string of data written to selected device
###########################################################################################################
sub write_dev {
  my $self     = shift;
  my $seldev   = shift;                        # scan chain position, beginning with 0
  my $type     = shift;                        # IRSHIFT or DRSHIFT
  my $data     = shift;                        # expected as string of binary data
  my $endstate = shift;                        # state to leave device in

  # Trap unspecified endstate
  if (not $endstate) {
    $endstate = RUN_TEST_IDLE_STATE;
  }

  #print "data to write to dev $seldev = $data\n";

  # convert data to long words
  my $data_lw = pack('L*', oct('0b'.$data));

  # Perform the USB operation
  my $ftc_status = $self->{fh}->{write}->Call($self->{key}, $type, length($data), $data_lw, $ftdi_buffer_size, $endstate);

  # Check return status
  die ("ERROR: unable to write\n") if ($ftc_status);

  return;
}

###########################################################################################################
# Read 32 bits from data register of selected device
# Usage: read_dev_dr($self, $deviceid, [$end_state]);
# Return: hex string of data read from selected device
###########################################################################################################
sub read_dev_dr {
  return read_dev(shift, shift, DRSHIFT, shift);
}

###########################################################################################################
# Read from instruction register of selected device
# Usage: read_dev_ir($self, $deviceid, [$end_state]);
# Return: hex string of data read from selected device
# Reading from IR will clear its contents unless it's written back!
###########################################################################################################
#sub read_dev_ir {
#  return read_dev(shift, shift, IRSHIFT, shift);
#}

###########################################################################################################
# Readfrom specified JTAG scan chain device
# Usage: read_dev($self, $deviceid, $type (IRSHIFT or DRSHIFT), [$endstate]);
# Return: hex string of data read from selected device
###########################################################################################################
sub read_dev {
  my $self       = shift;
  my $seldev      = shift;                        # scan chain position, beginning with 0
  my $type       = shift;                        # IRSHIFT or DRSHIFT
  my $endstate   = shift;                        # state to leave device in, optional

  my $rbuffer_lw = ' 'x$ftdi_buffer_size;        # allocate memory for read string
  my $nbytes_lw  = ' 'x4;                        # allocate memory for number of bytes read

  # Trap unspecified endstate
  if (not $endstate) {
    $endstate = RUN_TEST_IDLE_STATE;
  }

  # The number of bits to read from the specified device
  my $nbits = 0;
  if ($type eq IRSHIFT) {
    $nbits = length($self->{di}->[$seldev]->{ircmds}->{idcode}); # the size of the selected device's instruction register
  } else {
    $nbits = 32; #assumes data register width of 32
  }

  #printf("Reading %d bits from device %d\n", $nbits, $seldev);

  # The first bit of data we want is sitting on TDO of the selected device.  If there are any devices in
  # the chain after the selected device, the data we want must get through those subsequent devices.
  # If this is a DRSHIFT, we assume each non-selected device has been given the BYPASS command, thus each
  # non-selected device will place the 1-bit BYPASS register on the chain.  If this is an IRSHIFT, each
  # non-selected device will require N extra bits, where N is the size of the device's instruction register.
  # This means that if there are N devices in the chain after the selected device, and the number of bits
  # of data being shifted is M, the total number of shifts must be:
  #   1*N+M for DRSHIFT
  #   I(X)*N+M for IRSHIFT, where I is the size of the instruction register for device X
  # Finally, the first N bits (in case of DRSHIFT) or I(X)*N (in case of IRSHIFT) of data received on TDO
  # should be discarded.
  my $nscdevs = scalar(@{$self->{di}});       # number of devices in scan chain

  # Calculate number of extra bits to shift
  my $nbits_extra = 0;
  foreach my $dev ($seldev+1 .. $nscdevs-1) {
    my $regsize = ($type eq DRSHIFT)? 1 : length($self->{di}->[$dev]->{ircmds}->{idcode});
    $nbits_extra += $regsize;
  }

  #printf("Reading %d extra bits from device %d\n", $nbits_extra, $seldev);

  # Perform the USB operation
  my $ftc_status = $self->{fh}->{read}->Call($self->{key}, DRSHIFT, $nbits + $nbits_extra, $rbuffer_lw, $nbytes_lw, $endstate);

  # Check return status
  die(sprintf("ERROR : %s : %s\n", 'Unable to read via JTAGkey', $ftc_status_type_aref->[$ftc_status])) if ($ftc_status);

  # Calculate the number of long words to convert (each long word is 32 bytes)
  my $nlw = int(($nbits+$nbits_extra-1)/32) + 1;

  # Unpack long words into binary string
  my $rdata_bstr = '';
  foreach my $d (unpack('L'x$nlw, $rbuffer_lw)) {
    $rdata_bstr = sprintf("%032b",$d) . $rdata_bstr;
  }

  #printf("Binary read data: %s\n", $rdata_bstr);

  return(sprintf "%08x", oct('0b'.substr($rdata_bstr, -1*($nbits+$nbits_extra), $nbits)));

}



###########################################################################################################
# Read the IDCODEs of the devices on the scan chain and check them against the IDCODEs defined in the
# JTAG object to verify that they are correct.
###########################################################################################################
sub verify_chain {
  my $self       = shift;

  # Allocate memory
  my $rbuffer_lw = ' 'x$ftdi_buffer_size; # allocate memory for read string
  my $nbytes_lw  = ' 'x4;                 # allocate memory for number of bytes read

  # Send JTAG devices to TEST LOGIC RESET state with a dummy write
  write_dev_ir($self, 0, 'idcode', TEST_LOGIC_STATE);

  my $idcode = '';
  my $dev = 0;
  my @idcodes = ();

  # Read IDCODEs, up to a max of 10 (this is arbitrary), until all zeroes are received
  while ($idcode ne "00000000" and $dev < 10) {

    # Shift 32 bits through data registers, be sure to end in the PAUSE-DR state.  If we return to the RUN-TEST-IDLE state
    # then each device will reload its IDCODE data register.
    my $ftc_status = $self->{fh}->{read}->Call($self->{key}, DRSHIFT, 32, $rbuffer_lw, $nbytes_lw, PAUSE_TEST_DATA_REGISTER_STATE);

    # Check return status
    die(sprintf("ERROR : %s : %s\n", 'Unable to read via JTAGkey', $ftc_status_type_aref->[$ftc_status])) if ($ftc_status);

    # Unpack long words into hex string
    $idcode = sprintf("%08x", unpack('L', $rbuffer_lw));

    # Add current IDCODE to array of IDCODEs
    push(@idcodes, $idcode) if $idcode ne "00000000";
  }

  # Reorder array of IDCODEs so that device 0 is the first one in the chain
  @idcodes = reverse(@idcodes);

  # Verify that IDCODEs read from the chain match the information in the chain description
  # This could alternately be used to assign the device information in the chain description
  foreach my $dev (0..scalar(@{$self->{di}})-1) {
    my $exp_idcode_re = $self->{di}->[$dev]->{idregx};
    my $rxd_idcode = $idcodes[$dev];

    if ($rxd_idcode !~ /$exp_idcode_re/) {
      die(sprintf("ERROR : IDCODE for device %d (%s) does not match defined regular expression (%s)", $dev, $rxd_idcode, $exp_idcode_re));
    }
  }

  print "IDCODES verified\n";
  foreach my $dev (0..scalar(@{$self->{di}})-1) {
    printf("DEVICE %d : %s\n", $dev, $self->{di}->[$dev]->{name});
  }

  # Send JTAG devices to TEST LOGIC RESET state with a dummy write
  write_dev_ir($self, 0, 'idcode', TEST_LOGIC_STATE);

  return;
}


###########################################################################################################
# Generate $nclks clock pulses
###########################################################################################################
sub genclks {
  my $self  = shift;
  my $nclks = shift;

  my $ftc_status = $self->{fh}->{gen_clks}->Call($self->{key}, $nclks);
  die("Unable to generate clocks\n") if ($ftc_status);
  return;
}

1;
__END__
# Below is stub documentation for your module. You'd better edit it!

=head1 NAME

Device::Jtag::USB::FTCJTAG - Perl extension for communicating with JTAG devices
using the FTDI FTCJTAG driver.

=head1 SYNOPSIS

  use Device::Jtag::USB::FTCJTAG;
  my $jtag = Device::Jtag::USB::FTCJTAG->new();
  $jtag->write_dev_ir(0, 'user1');
  $jtag->write_dev_dr(0, $string_of_bits);

=head1 DESCRIPTION

A JTAG device driver for Perl using the FTDI Chip FTCJTAG driver (see
L<http://ftdichip.com/Projects/MPSSE/FTCJTAG.htm>).  The driver is designed
for use with the FTDI Chip FT2232 USB UART/FIFO IC (see
L<http://ftdichip.com/Products/FT2232C.htm>).  A hardware device is required
which incorporates the FT2232 chip in a form that (a) connects to a PC's
USB port on one end and (b) connects to the JTAG interface of a target device
(or devices).  One such device is the Amontec JTAGKey (see
L<http://www.amontec.com/jtagkey.shtml>).


=head2 EXPORT

None by default.

=head2 CONSTRUCTOR

=over 4

=item new()

Creates the object.  After creating the object, the
device(s) on the JTAG scan chain must be defined.  This is done by assigning
to the 'di' (device info) hash key of the variable returned by the constructor.

For example, if the JTAG scan chain consists of a XC3S1200E FPGA followed by
a XCF04S Flash PROM, the chain is defined as follows:

  my $jtag = Device::Jtag::USB::FTCJTAG->new();
  $jtag->{di} = [                        # define device information for devices on scan chain
    {name   => 'XC3S1200E FPGA',         # device 0 nickname
     idregx => '^.1c2e093',              # device 0 idcode reg expression
     ircmds => {idcode => '001001',      # device 0 instructions
                user1  => '000010',
                user2  => '000011',
                bypass => '111111'}},

    {name   => 'XCF0XS Platform Flash',  # device 1 nickname
     idregx => '^.504.093',              # device 1 idcode reg expression
     ircmds => {idcode => '11111110',    # device 1 instructions
                bypass => '11111111'}}
  ];

The values for idregx and the instruction register commands are defined in
the target device's BSDL file.  These are typically available for download
from the manufacturer's website.  For BSDL models from Xilinx, visit
L<http://www.xilinx.com/xlnx/xil_sw_updates_home.jsp#BSDL Models>.  A regular
expression is used for the device's IDCODE because a given devices IDCODE
will vary based upon package type, etc.

=back

=head2 METHODS

=over 4

=item verify_chain()

Read the IDCODEs of the devices on the scan chain and check them against the IDCODEs defined in the
JTAG object to verify that they are correct.

=item write_dev_ir(DEVICE, INSTRUCTION, [ENDSTATE])

Writes INSTRUCTION to JTAG instruction register of device number DEVICE of the scan chain, leaving that device in ENDSTATE when done.
The INSTRUCTION must be the hash key of one of the instructions defined for DEVICE.  If no ENDSTATE is supplied in the function call,
a default of RUN_TEST_IDLE is assumed.

=item write_dev_dr(DEVICE, DATA, [ENDSTATE])

Writes DATA to JTAG data register of device number DEVICE of the scan chain, leaving that device in ENDSTATE when done.
DATA should be formatted as a string of 0s and 1s representing the binary value to be written to the device.
If no ENDSTATE is supplied in the function call, a default of RUN_TEST_IDLE is assumed.

=item read_dev_dr(DEVICE, [ENDSTATE])

Reads JTAG data register or device number DEVICE of the scan chain, leaving that device in ENDSTATE when done.
DATA is returned formatted as a string of 0s and 1s representing the binary value of the data.  The number of
bits read from the device is 32.  If no ENDSTATE is supplied in the function call, a default of RUN_TEST_IDLE is assumed.

=back


=head1 SEE ALSO

L<http://ftdichip.com/Projects/MPSSE/FTCJTAG.htm> FTDI Chip FTCJTAG driver page.

L<http://ftdichip.com/Products/FT2232C.htm> FTDI Chip FT2232 device page.

L<http://www.amontec.com/jtagkey.shtml> Amontec JTAGKey device page.

L<http://www.xilinx.com/bvdocs/appnotes/xapp139.pdf> Information on JTAG tap controllers.


=head1 TO DO

Add autodetection of scan chain devices and auto-assignment of IDCODE value and INSTRUCTION definitions from BSDL files.

=head1 HISTORY

=over

=item * 0.04  Sun May 27 2007
Changed to .tar.gz archive type.

=item * 0.03  Fri May 25 2007
Fixed prereqs in Makefile.PL

=item * 0.02  Sun May 21 2007
Removed autoloader from FTCJTAG.pm file

=item * 0.01  Sun May 20 2007
Original version. By default, assumes devices on JTAG scan chain are (0) Xilinx XC3S1200E FPGA and (1) Xilinx XCF04S Flash PROM.

=back

=head1 AUTHOR

Toby Deitrich L<http://toby.deitrich.net/>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2007 by Toby Deitrich

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.8 or,
at your option, any later version of Perl 5 you may have available.


=cut
