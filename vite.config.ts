import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import path from "path";

// https://vite.dev/config/
export default defineConfig({
  plugins: [react()],
  root: "src-ts",
  base: "/ai-explorations",
  build: {
    outDir: "../dist",
  },
  server: {
    fs: {
      allow: [
        path.resolve(__dirname, "src-ts"),
        path.resolve(__dirname, "output"),
      ],
    },
    watch: {
      include: [path.resolve(__dirname, "output/**/*.js")],
      //awaitWriteFinish: { stabilityThreshold: 150, pollInterval: 20 },
    } as any,
    hmr: {},
  },
});
