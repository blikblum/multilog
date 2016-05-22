{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit multiloglaz;

{$warn 5023 off : no warning about unused units}
interface

uses
  MultiLog, registermultilog, logtreeview, LCLLogger, filechannel, IPCChannel, 
  MultiLogLCLHelpers, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('registermultilog', @registermultilog.Register);
end;

initialization
  RegisterPackage('multiloglaz', @Register);
end.
