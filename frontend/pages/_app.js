import { ThemeProvider } from "@mui/material/styles";
import "setimmediate";
import Head from "next/head";
import Router from "next/router";
import NProgress from "nprogress";
import "../../node_modules/nprogress/nprogress.css";
import React from "react";
import "../styles/globals.css";
import { theme } from "../theme";

Router.events.on("routeChangeStart", () => {
  console.log("routeChangeStart");
  NProgress.start();
});
Router.events.on("routeChangeComplete", () => {
  console.log("routeChangeComplete");
  NProgress.done();
});
Router.events.on("routeChangeError", () => {
  console.log("routeChangeError");
  // NProgress.done();
});

function MyApp({ Component, pageProps }) {
  React.useEffect(() => {
    // Remove the server-side injected CSS.
    const jssStyles = document.querySelector("#jss-server-side");
    if (jssStyles) {
      jssStyles.parentElement.removeChild(jssStyles);
    }
  }, []);

  return (
    <React.Fragment>
      <Head>
        <title>Riju</title>
        <meta
          name="viewport"
          content="minimum-scale=1, initial-scale=1, width=device-width"
        />
      </Head>
      <ThemeProvider theme={theme}>
        <Component {...pageProps} />
      </ThemeProvider>
    </React.Fragment>
  );
}

export default MyApp;
