{pkgs, ...}:
{
  home.packages = with pkgs; [
    matrixcli
  ];

  home.file.".config/matrixcli/config.py".text = ''
    def password_eval():
        return "testtest"

    accounts=[{ "server": "matrix.org",
                "username": "vcmiraldo",
                "passeval": password_eval
              }]
  '';
}
