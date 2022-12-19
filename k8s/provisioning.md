```bash
curl -sSLf https://get.k0s.sh | sudo sh
sudo mkdir /etc/k0s
k0s config create > /etc/k0s/k0s.yaml
```

Edit to have this config:

```yaml
spec:
  extensions:
    storage:
      type: openebs_local_storage
```

```bash
sudo k0s install controller --single
sudo k0s start
```

Go to client machine:

```bash
ssh riju-k8s sudo -S k0s kubeconfig admin > ~/.kube/config
```
