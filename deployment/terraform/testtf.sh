TF_VAR_marlowe_github_client_id=$(pass david/marlowe/githubClientId)
TF_VAR_marlowe_github_client_secret=$(pass david/marlowe/githubClientSecret)
TF_VAR_marlowe_jwt_signature=$(pass david/marlowe/jwtSignature)
TF_VAR_plutus_github_client_id=$(pass david/plutus/githubClientId)
TF_VAR_plutus_github_client_secret=$(pass david/plutus/githubClientSecret)
TF_VAR_plutus_jwt_signature=$(pass david/plutus/jwtSignature)

export TF_VAR_marlowe_github_client_id
export TF_VAR_marlowe_github_client_secret
export TF_VAR_marlowe_jwt_signature
export TF_VAR_plutus_github_client_id
export TF_VAR_plutus_github_client_secret
export TF_VAR_plutus_jwt_signature

/nix/store/qk0cmfvkyrzfb0a4bjcszglidd9ba56z-terraform-0.12.29/bin/terraform init
/nix/store/qk0cmfvkyrzfb0a4bjcszglidd9ba56z-terraform-0.12.29/bin/terraform workspace select david
/nix/store/qk0cmfvkyrzfb0a4bjcszglidd9ba56z-terraform-0.12.29/bin/terraform apply -var-file=david.tfvars
