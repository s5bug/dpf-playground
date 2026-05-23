import path from 'node:path'
import scalaJSPlugin from '@scala-js/vite-plugin-scalajs'
import { defineConfig, loadEnv } from 'vite'

// https://vitejs.dev/config/
export default defineConfig(({ mode }) => {
  const env = loadEnv(mode, process.cwd(), '')
  let basePath: string
  if (env.GITHUB_ACTIONS === 'true') {
    basePath = `/${env.GITHUB_REPOSITORY.split('/')[1]}`
  } else {
    basePath = '/'
  }
  return {
    base: basePath,
    resolve: {
      alias: {
        '@': path.resolve(__dirname, './src/'),
      },
    },
    plugins: [
      scalaJSPlugin({
        cwd: './scalajs',
      }),
    ],
  }
})
