let
  yourPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDKmZDkC1AJ2wiOnWlQZ68qhcX5+BqdvdAoo5Lyor2FZ";
in
{
  "work-servers-data.age".publicKeys = [ yourPublicKey ];
}
