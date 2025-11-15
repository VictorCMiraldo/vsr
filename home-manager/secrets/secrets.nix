let
  # This is the public-key from ~/keychain/vsr-secrets/id_ed25519; editing secrets will require
  # you to pass the identity file. So, to edit work-servers-data.age, the command is:
  #
  # > agenix -e work-servers-data.age -i ~/keychain/vsr-secrets/id_ed25519
  #
  pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDKmZDkC1AJ2wiOnWlQZ68qhcX5+BqdvdAoo5Lyor2FZ";
in
{
  "work-servers-data.age".publicKeys = [ pubkey ];
}
