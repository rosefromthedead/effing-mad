#![feature(generators)]
#![feature(generator_trait)]

use effing_mad::{effectful, handle_group, handle_group_async, handler, run};

fn main() {
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();
    rt.block_on(interesting_and_useful());

    boring_and_old_fashioned();
}

// very complex and powerful API
effing_mad::effects! {
    HttpRequest {
        fn get(url: &'static str) -> String;
    }
}

// this function does not specify whether the request happens synchronously or asynchronously
#[effectful(HttpRequest)]
fn example() -> usize {
    let body = yield HttpRequest::get("http://example.com");
    body.len()
}

async fn interesting_and_useful() {
    let handler = handler! {
        async HttpRequest {
            get(url) => reqwest::get(url).await.unwrap().text().await.unwrap(),
        }
    };

    let req1 = handle_group_async(example(), handler);
    let req2 = handle_group_async(example(), handler);

    // asyncified effectful functions can be composed in the same ways as traditional futures
    let (res1, res2) = futures::future::join(req1, req2).await;
    println!("asynchronously found {res1} and {res2} bytes");
}

fn boring_and_old_fashioned() {
    let handler = handler! {
        HttpRequest {
            get(url) => reqwest::blocking::get(url).unwrap().text().unwrap(),
        }
    };

    let req = handle_group(example(), handler);
    let res = run(req);
    println!("synchronously found {res} bytes");
}
