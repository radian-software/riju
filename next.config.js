// const isProd = process.env.NODE_ENV === "production";
const isProd = false;

module.exports = {
  // assetPrefix: isProd ? "/riju" : "",
  // assetPrefix: "/riju",
  eslint: {
    // Warning: This allows production builds to successfully complete even if
    // your project has ESLint errors.
    ignoreDuringBuilds: true,
  },
  basePath: isProd ? "/" : "",
  reactStrictMode: true,
  trailingSlash: isProd ? true : false,
  webpack: (config, { buildId, dev, isServer, defaultLoaders, webpack }) => {
    if (!isServer) {
      config.resolve.fallback.net = false;
    }
    config.resolve.alias.vscode = require.resolve(
      "monaco-languageclient/lib/vscode-compatibility"
    );

    return config;
  },
};
