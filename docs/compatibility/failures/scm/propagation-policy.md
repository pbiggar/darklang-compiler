# scm/propagation-policy.dark

[Source fixture](../../../../test/fixtures/e2e/upstream/scm/propagation-policy.dark) · [File list](../../current-audit.md)

Executed 19 assertions: **0 passed, 19 failed**.
Of 19 previously disabled assertions, **0 passed and 19 failed**.

| Test | Observed failure |
| --- | --- |
| [L45](../../../../test/fixtures/e2e/upstream/scm/propagation-policy.dark#L45) — Darklang.SCM.Propagation.candidateKeys (Helpers.loc [ "A", "B", "C" ] "f") | Unknown function or value 'Darklang.SCM.Propagation.candidateKeys' |
| [L48](../../../../test/fixtures/e2e/upstream/scm/propagation-policy.dark#L48) — Darklang.SCM.Propagation.candidateKeys (Helpers.loc [ "A" ] "f") | Unknown function or value 'Darklang.SCM.Propagation.candidateKeys' |
| [L52](../../../../test/fixtures/e2e/upstream/scm/propagation-policy.dark#L52) — (Darklang.SCM.Propagation.candidateKeys (Helpers.loc [] "f")) | Unknown function or value 'Darklang.SCM.Propagation.candidateKeys' |
| [L58](../../../../test/fixtures/e2e/upstream/scm/propagation-policy.dark#L58) — Darklang.SCM.Propagation.candidateKeys (Helpers.loc [ "A", "B" ] "") | Unknown function or value 'Darklang.SCM.Propagation.candidateKeys' |
| [L65](../../../../test/fixtures/e2e/upstream/scm/propagation-policy.dark#L65) — (Helpers.follows [] [ "A" ] "f") | Unknown function or value 'Helpers.follows' |
| [L67](../../../../test/fixtures/e2e/upstream/scm/propagation-policy.dark#L67) — Darklang.SCM.Propagation.explicitPolicyIn [] (Helpers.loc [ "A" ] "f") | Unknown function or value 'Darklang.SCM.Propagation.explicitPolicyIn' |
| [L73](../../../../test/fixtures/e2e/upstream/scm/propagation-policy.dark#L73) — Helpers.follows [ Helpers.choice "A" "f" Darklang.SCM.Propagation.Policy.Pin Helpers.choice "A" "" Darklang... | Unknown function or value 'Helpers.follows' |
| [L81](../../../../test/fixtures/e2e/upstream/scm/propagation-policy.dark#L81) — Helpers.follows [ Helpers.choice "A" "f" Darklang.SCM.Propagation.Policy.Follow Helpers.choice "A" "" Darkl... | Unknown function or value 'Helpers.follows' |
| [L88](../../../../test/fixtures/e2e/upstream/scm/propagation-policy.dark#L88) — Helpers.follows [ Helpers.choice "A.B" "" Darklang.SCM.Propagation.Policy.Pin Helpers.choice "A" "" Darklan... | Unknown function or value 'Helpers.follows' |
| [L95](../../../../test/fixtures/e2e/upstream/scm/propagation-policy.dark#L95) — Helpers.follows [ Helpers.choice "" "" Darklang.SCM.Propagation.Policy.Pin ] [ "A", "B" ] "f" | Unknown function or value 'Helpers.follows' |
| [L101](../../../../test/fixtures/e2e/upstream/scm/propagation-policy.dark#L101) — Helpers.follows [ Helpers.choice "A" "" Darklang.SCM.Propagation.Policy.Pin ] [ "A", "B" ] "f" | Unknown function or value 'Helpers.follows' |
| [L109](../../../../test/fixtures/e2e/upstream/scm/propagation-policy.dark#L109) — Helpers.follows [ Helpers.choice "A" "g" Darklang.SCM.Propagation.Policy.Pin ] [ "A" ] "f" | Unknown function or value 'Helpers.follows' |
| [L116](../../../../test/fixtures/e2e/upstream/scm/propagation-policy.dark#L116) — Darklang.SCM.Propagation.shouldFollowIn [ Darklang.SCM.Propagation.Choice { owner = "Other" modules = "A" n... | Unknown function or value 'Darklang.SCM.Propagation.shouldFollowIn' |
| [L129](../../../../test/fixtures/e2e/upstream/scm/propagation-policy.dark#L129) — (Darklang.SCM.Propagation.policyToString Darklang.SCM.Propagation.Policy.Pin) | Unknown function or value 'Darklang.SCM.Propagation.policyToString' |
| [L130](../../../../test/fixtures/e2e/upstream/scm/propagation-policy.dark#L130) — (Darklang.SCM.Propagation.policyToString Darklang.SCM.Propagation.Policy.Follow) | Unknown function or value 'Darklang.SCM.Propagation.policyToString' |
| [L132](../../../../test/fixtures/e2e/upstream/scm/propagation-policy.dark#L132) — Darklang.SCM.Propagation.policyFromString "pin" | Unknown function or value 'Darklang.SCM.Propagation.policyFromString' |
| [L135](../../../../test/fixtures/e2e/upstream/scm/propagation-policy.dark#L135) — Darklang.SCM.Propagation.policyFromString "follow" | Unknown function or value 'Darklang.SCM.Propagation.policyFromString' |
| [L140](../../../../test/fixtures/e2e/upstream/scm/propagation-policy.dark#L140) — (Darklang.SCM.Propagation.policyFromString "unset") | Unknown function or value 'Darklang.SCM.Propagation.policyFromString' |
| [L141](../../../../test/fixtures/e2e/upstream/scm/propagation-policy.dark#L141) — (Darklang.SCM.Propagation.policyFromString "") | Unknown function or value 'Darklang.SCM.Propagation.policyFromString' |
