// Copyright 2013 The Flutter Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

// Allow access to fml::MessageLoop::GetCurrent() in order to flush platform
// thread tasks.
#define FML_USED_ON_EMBEDDER

#include <functional>
#include <type_traits>

#include "flutter/fml/macros.h"
#include "flutter/fml/message_loop.h"
#include "flutter/fml/synchronization/waitable_event.h"
#include "flutter/lib/ui/semantics/semantics_node.h"
#include "flutter/shell/platform/embedder/embedder.h"
#include "flutter/shell/platform/embedder/tests/embedder_config_builder.h"
#include "flutter/testing/testing.h"
#include "third_party/tonic/converter/dart_converter.h"

#include "gmock/gmock.h"  // For EXPECT_THAT and matchers
#include "gtest/gtest.h"

// CREATE_NATIVE_ENTRY is leaky by design
// NOLINTBEGIN(clang-analyzer-core.StackAddressEscape)

namespace flutter {
namespace testing {

using EmbedderA11yTest = testing::EmbedderTest;
using ::testing::ElementsAre;

constexpr static char kTooltip[] = "tooltip";

TEST_F(EmbedderTest, CannotProvideMultipleSemanticsCallbacks) {
  {
    EmbedderConfigBuilder builder(
        GetEmbedderContext(EmbedderTestContextType::kSoftwareContext));
    builder.SetSoftwareRendererConfig();
    builder.GetProjectArgs().update_semantics_callback =
        [](const FlutterSemanticsUpdate* update, void* user_data) {};
    builder.GetProjectArgs().update_semantics_callback2 =
        [](const FlutterSemanticsUpdate2* update, void* user_data) {};
    auto engine = builder.InitializeEngine();
    ASSERT_FALSE(engine.is_valid());
    engine.reset();
  }

  {
    EmbedderConfigBuilder builder(
        GetEmbedderContext(EmbedderTestContextType::kSoftwareContext));
    builder.SetSoftwareRendererConfig();
    builder.GetProjectArgs().update_semantics_callback2 =
        [](const FlutterSemanticsUpdate2* update, void* user_data) {};
    builder.GetProjectArgs().update_semantics_node_callback =
        [](const FlutterSemanticsNode* update, void* user_data) {};
    builder.GetProjectArgs().update_semantics_custom_action_callback =
        [](const FlutterSemanticsCustomAction* update, void* user_data) {};
    auto engine = builder.InitializeEngine();
    ASSERT_FALSE(engine.is_valid());
    engine.reset();
  }

  {
    EmbedderConfigBuilder builder(
        GetEmbedderContext(EmbedderTestContextType::kSoftwareContext));
    builder.SetSoftwareRendererConfig();
    builder.GetProjectArgs().update_semantics_callback =
        [](const FlutterSemanticsUpdate* update, void* user_data) {};
    builder.GetProjectArgs().update_semantics_node_callback =
        [](const FlutterSemanticsNode* update, void* user_data) {};
    builder.GetProjectArgs().update_semantics_custom_action_callback =
        [](const FlutterSemanticsCustomAction* update, void* user_data) {};
    auto engine = builder.InitializeEngine();
    ASSERT_FALSE(engine.is_valid());
    engine.reset();
  }

  {
    EmbedderConfigBuilder builder(
        GetEmbedderContext(EmbedderTestContextType::kSoftwareContext));
    builder.SetSoftwareRendererConfig();
    builder.GetProjectArgs().update_semantics_callback2 =
        [](const FlutterSemanticsUpdate2* update, void* user_data) {};
    builder.GetProjectArgs().update_semantics_callback =
        [](const FlutterSemanticsUpdate* update, void* user_data) {};
    builder.GetProjectArgs().update_semantics_node_callback =
        [](const FlutterSemanticsNode* update, void* user_data) {};
    builder.GetProjectArgs().update_semantics_custom_action_callback =
        [](const FlutterSemanticsCustomAction* update, void* user_data) {};
    auto engine = builder.InitializeEngine();
    ASSERT_FALSE(engine.is_valid());
    engine.reset();
  }
}

template <typename... Ts>
class NativeFunction {
 public:
  NativeFunction() = default;
  ~NativeFunction() = default;

  void SetHandler(std::function<void(Ts...)> function) {
    function_ = std::move(function);
  }

  void Wait() { platform_thread_latch.Wait(); }

  void Invoke(Dart_NativeArguments args) {
    InvokeWithConvertedArgs(args, std::make_index_sequence<sizeof...(Ts)>{});
    platform_thread_latch.Signal();
  }

  operator std::function<void(Dart_NativeArguments)>() {
    return std::bind(&NativeFunction<Ts...>::Invoke, this,
                     std::placeholders::_1);
  }

 private:
  // Evaluates to the Ith type of Ts.
  template <int I>
  using NthTypeOf = typename std::tuple_element<I, std::tuple<Ts...>>::type;

  // Converts the Ith argument in args to a value of type T.
  template <int I, typename T>
  static T Convert(Dart_NativeArguments args) {
    Dart_Handle exception = nullptr;
    return ::tonic::DartConverter<T>::FromArguments(args, I, exception);
  }

  // Invokes function_, passing each of the args through Convert.
  template <size_t... Is>
  void InvokeWithConvertedArgs(Dart_NativeArguments args,
                               std::index_sequence<Is...>) {
    function_(Convert<Is, NthTypeOf<Is>>(args)...);
  }

  fml::AutoResetWaitableEvent platform_thread_latch;
  std::function<void(Ts...)> function_;
};

TEST_F(EmbedderA11yTest, A11yTreeIsConsistent) {
#if defined(OS_FUCHSIA)
  GTEST_SKIP() << "This test crashes on Fuchsia. https://fxbug.dev/87493 ";
#endif  // OS_FUCHSIA

  auto& context = GetEmbedderContext(EmbedderTestContextType::kSoftwareContext);

  // Called by the Dart text fixture on the UI thread to signal that the C++
  // unittest should resume.
  // TODO(cbracken): This is unused in this test.
  NativeFunction<> notify_native_test;
  context.AddNativeCallback("SignalNativeTest",
                            CREATE_NATIVE_ENTRY(notify_native_test));
  notify_native_test.SetHandler([] {});

  // Called by test fixture on UI thread to pass data back to this test.
  NativeFunction<bool> notify_semantics_enabled;
  context.AddNativeCallback("NotifySemanticsEnabled",
                            CREATE_NATIVE_ENTRY(notify_semantics_enabled));

  NativeFunction<bool> notify_accessibility_features;
  context.AddNativeCallback("NotifyAccessibilityFeatures",
                            CREATE_NATIVE_ENTRY(notify_accessibility_features));

  NativeFunction<int64_t, int64_t, std::vector<int64_t>>
      notify_semantics_action;
  context.AddNativeCallback("NotifySemanticsAction",
                            CREATE_NATIVE_ENTRY(notify_semantics_action));

  fml::AutoResetWaitableEvent semantics_update_latch;
  context.SetSemanticsUpdateCallback2(
      [&](const FlutterSemanticsUpdate2* update) {
        ASSERT_EQ(size_t(4), update->node_count);
        ASSERT_EQ(size_t(1), update->custom_action_count);

        for (size_t i = 0; i < update->node_count; i++) {
          const FlutterSemanticsNode2* node = update->nodes[i];

          ASSERT_EQ(1.0, node->transform.scaleX);
          ASSERT_EQ(2.0, node->transform.skewX);
          ASSERT_EQ(3.0, node->transform.transX);
          ASSERT_EQ(4.0, node->transform.skewY);
          ASSERT_EQ(5.0, node->transform.scaleY);
          ASSERT_EQ(6.0, node->transform.transY);
          ASSERT_EQ(7.0, node->transform.pers0);
          ASSERT_EQ(8.0, node->transform.pers1);
          ASSERT_EQ(9.0, node->transform.pers2);
          ASSERT_EQ(std::strncmp(kTooltip, node->tooltip, sizeof(kTooltip) - 1),
                    0);

          if (node->id == 128) {
            ASSERT_EQ(0x3f3, node->platform_view_id);
          } else {
            ASSERT_NE(kFlutterSemanticsNodeIdBatchEnd, node->id);
            ASSERT_EQ(0, node->platform_view_id);
          }
        }

        semantics_update_latch.Signal();
      });

  EmbedderConfigBuilder builder(context);
  builder.SetSoftwareRendererConfig();
  builder.SetDartEntrypoint("a11y_main");

  // 1: Wait for initial notifySemanticsEnabled(false).
  notify_semantics_enabled.SetHandler(
      [](bool enabled) { ASSERT_FALSE(enabled); });
  auto engine = builder.LaunchEngine();
  ASSERT_TRUE(engine.is_valid());
  notify_semantics_enabled.Wait();

  // 2: Enable semantics. Wait for notifySemanticsEnabled(true).
  // 3: Wait for notifyAccessibilityFeatures (reduce_motion == false)
  notify_semantics_enabled.SetHandler(
      [](bool enabled) { ASSERT_TRUE(enabled); });
  notify_accessibility_features.SetHandler(
      [](bool reduce_motion) { ASSERT_FALSE(reduce_motion); });
  auto result = FlutterEngineUpdateSemanticsEnabled(engine.get(), true);
  notify_semantics_enabled.Wait();
  notify_accessibility_features.Wait();

  // 4: Wait for notifyAccessibilityFeatures (reduce_motion == true)
  notify_accessibility_features.SetHandler(
      [](bool reduce_motion) { ASSERT_TRUE(reduce_motion); });
  result = FlutterEngineUpdateAccessibilityFeatures(
      engine.get(), kFlutterAccessibilityFeatureReduceMotion);
  ASSERT_EQ(result, FlutterEngineResult::kSuccess);
  notify_accessibility_features.Wait();

  // 5: Wait for UpdateSemantics callback on platform (current) thread.
  notify_native_test.Wait();
  fml::MessageLoop::GetCurrent().RunExpiredTasksNow();
  semantics_update_latch.Wait();

  // 6: Dispatch a tap to semantics node 42. Wait for NotifySemanticsAction.
  notify_semantics_action.SetHandler([](int64_t node_id, int64_t action_id,
                                        std::vector<int64_t> arguments) {
    ASSERT_EQ(42, node_id);
    ASSERT_EQ(static_cast<int32_t>(flutter::SemanticsAction::kTap), action_id);
    ASSERT_THAT(arguments, ElementsAre(2, 1));
  });
  std::vector<uint8_t> bytes({2, 1});
  result = FlutterEngineDispatchSemanticsAction(
      engine.get(), 42, kFlutterSemanticsActionTap, bytes.data(), bytes.size());
  ASSERT_EQ(result, FlutterEngineResult::kSuccess);
  notify_semantics_action.Wait();

  // 7: Disable semantics. Wait for NotifySemanticsEnabled(false).
  notify_semantics_enabled.SetHandler(
      [](bool enabled) { ASSERT_FALSE(enabled); });
  result = FlutterEngineUpdateSemanticsEnabled(engine.get(), false);
  ASSERT_EQ(result, FlutterEngineResult::kSuccess);
  notify_semantics_enabled.Wait();
}

TEST_F(EmbedderA11yTest, A11yTreeIsConsistentUsingUnstableCallbacks) {
#if defined(OS_FUCHSIA)
  GTEST_SKIP() << "This test crashes on Fuchsia. https://fxbug.dev/87493 ";
#endif  // OS_FUCHSIA

  auto& context = GetEmbedderContext(EmbedderTestContextType::kSoftwareContext);

  fml::AutoResetWaitableEvent signal_native_latch;

  // Called by the Dart text fixture on the UI thread to signal that the C++
  // unittest should resume.
  context.AddNativeCallback(
      "SignalNativeTest",
      CREATE_NATIVE_ENTRY(([&signal_native_latch](Dart_NativeArguments) {
        signal_native_latch.Signal();
      })));

  // Called by test fixture on UI thread to pass data back to this test.
  NativeEntry notify_semantics_enabled_callback;
  context.AddNativeCallback(
      "NotifySemanticsEnabled",
      CREATE_NATIVE_ENTRY(
          ([&notify_semantics_enabled_callback](Dart_NativeArguments args) {
            ASSERT_NE(notify_semantics_enabled_callback, nullptr);
            notify_semantics_enabled_callback(args);
          })));

  NativeEntry notify_accessibility_features_callback;
  context.AddNativeCallback(
      "NotifyAccessibilityFeatures",
      CREATE_NATIVE_ENTRY((
          [&notify_accessibility_features_callback](Dart_NativeArguments args) {
            ASSERT_NE(notify_accessibility_features_callback, nullptr);
            notify_accessibility_features_callback(args);
          })));

  NativeEntry notify_semantics_action_callback;
  context.AddNativeCallback(
      "NotifySemanticsAction",
      CREATE_NATIVE_ENTRY(
          ([&notify_semantics_action_callback](Dart_NativeArguments args) {
            ASSERT_NE(notify_semantics_action_callback, nullptr);
            notify_semantics_action_callback(args);
          })));

  fml::AutoResetWaitableEvent semantics_update_latch;
  context.SetSemanticsUpdateCallback([&](const FlutterSemanticsUpdate* update) {
    ASSERT_EQ(size_t(4), update->nodes_count);
    ASSERT_EQ(size_t(1), update->custom_actions_count);

    for (size_t i = 0; i < update->nodes_count; i++) {
      const FlutterSemanticsNode* node = update->nodes + i;

      ASSERT_EQ(1.0, node->transform.scaleX);
      ASSERT_EQ(2.0, node->transform.skewX);
      ASSERT_EQ(3.0, node->transform.transX);
      ASSERT_EQ(4.0, node->transform.skewY);
      ASSERT_EQ(5.0, node->transform.scaleY);
      ASSERT_EQ(6.0, node->transform.transY);
      ASSERT_EQ(7.0, node->transform.pers0);
      ASSERT_EQ(8.0, node->transform.pers1);
      ASSERT_EQ(9.0, node->transform.pers2);
      ASSERT_EQ(std::strncmp(kTooltip, node->tooltip, sizeof(kTooltip) - 1), 0);

      if (node->id == 128) {
        ASSERT_EQ(0x3f3, node->platform_view_id);
      } else {
        ASSERT_NE(kFlutterSemanticsNodeIdBatchEnd, node->id);
        ASSERT_EQ(0, node->platform_view_id);
      }
    }

    semantics_update_latch.Signal();
  });

  EmbedderConfigBuilder builder(context);
  builder.SetSoftwareRendererConfig();
  builder.SetDartEntrypoint("a11y_main");

  auto engine = builder.LaunchEngine();
  ASSERT_TRUE(engine.is_valid());

  // 1: Wait for initial notifySemanticsEnabled(false).
  fml::AutoResetWaitableEvent notify_semantics_enabled_latch;
  notify_semantics_enabled_callback = [&](Dart_NativeArguments args) {
    Dart_Handle exception = nullptr;
    bool enabled =
        ::tonic::DartConverter<bool>::FromArguments(args, 0, exception);
    ASSERT_FALSE(enabled);
    notify_semantics_enabled_latch.Signal();
  };
  notify_semantics_enabled_latch.Wait();

  // Prepare notifyAccessibilityFeatures callback.
  fml::AutoResetWaitableEvent notify_features_latch;
  notify_accessibility_features_callback = [&](Dart_NativeArguments args) {
    Dart_Handle exception = nullptr;
    bool enabled =
        ::tonic::DartConverter<bool>::FromArguments(args, 0, exception);
    ASSERT_FALSE(enabled);
    notify_features_latch.Signal();
  };

  // 2: Enable semantics. Wait for notifySemanticsEnabled(true).
  fml::AutoResetWaitableEvent notify_semantics_enabled_latch_2;
  notify_semantics_enabled_callback = [&](Dart_NativeArguments args) {
    Dart_Handle exception = nullptr;
    bool enabled =
        ::tonic::DartConverter<bool>::FromArguments(args, 0, exception);
    ASSERT_TRUE(enabled);
    notify_semantics_enabled_latch_2.Signal();
  };
  auto result = FlutterEngineUpdateSemanticsEnabled(engine.get(), true);
  ASSERT_EQ(result, FlutterEngineResult::kSuccess);
  notify_semantics_enabled_latch_2.Wait();

  // 3: Wait for notifyAccessibilityFeatures (reduce_motion == false)
  notify_features_latch.Wait();

  // 4: Wait for notifyAccessibilityFeatures (reduce_motion == true)
  fml::AutoResetWaitableEvent notify_features_latch_2;
  notify_accessibility_features_callback = [&](Dart_NativeArguments args) {
    Dart_Handle exception = nullptr;
    bool enabled =
        ::tonic::DartConverter<bool>::FromArguments(args, 0, exception);
    ASSERT_TRUE(enabled);
    notify_features_latch_2.Signal();
  };
  result = FlutterEngineUpdateAccessibilityFeatures(
      engine.get(), kFlutterAccessibilityFeatureReduceMotion);
  ASSERT_EQ(result, FlutterEngineResult::kSuccess);
  notify_features_latch_2.Wait();

  // 5: Wait for UpdateSemantics callback on platform (current) thread.
  signal_native_latch.Wait();
  fml::MessageLoop::GetCurrent().RunExpiredTasksNow();
  semantics_update_latch.Wait();

  // 6: Dispatch a tap to semantics node 42. Wait for NotifySemanticsAction.
  fml::AutoResetWaitableEvent notify_semantics_action_latch;
  notify_semantics_action_callback = [&](Dart_NativeArguments args) {
    Dart_Handle exception = nullptr;
    int64_t node_id =
        ::tonic::DartConverter<int64_t>::FromArguments(args, 0, exception);
    ASSERT_EQ(42, node_id);

    int64_t action_id =
        ::tonic::DartConverter<int64_t>::FromArguments(args, 1, exception);
    ASSERT_EQ(static_cast<int32_t>(flutter::SemanticsAction::kTap), action_id);

    std::vector<int64_t> semantic_args =
        ::tonic::DartConverter<std::vector<int64_t>>::FromArguments(args, 2,
                                                                    exception);
    ASSERT_THAT(semantic_args, ElementsAre(2, 1));
    notify_semantics_action_latch.Signal();
  };
  std::vector<uint8_t> bytes({2, 1});
  result = FlutterEngineDispatchSemanticsAction(
      engine.get(), 42, kFlutterSemanticsActionTap, &bytes[0], bytes.size());
  ASSERT_EQ(result, FlutterEngineResult::kSuccess);
  notify_semantics_action_latch.Wait();

  // 7: Disable semantics. Wait for NotifySemanticsEnabled(false).
  fml::AutoResetWaitableEvent notify_semantics_enabled_latch_3;
  notify_semantics_enabled_callback = [&](Dart_NativeArguments args) {
    Dart_Handle exception = nullptr;
    bool enabled =
        ::tonic::DartConverter<bool>::FromArguments(args, 0, exception);
    ASSERT_FALSE(enabled);
    notify_semantics_enabled_latch_3.Signal();
  };
  result = FlutterEngineUpdateSemanticsEnabled(engine.get(), false);
  ASSERT_EQ(result, FlutterEngineResult::kSuccess);
  notify_semantics_enabled_latch_3.Wait();
}

TEST_F(EmbedderA11yTest, A11yTreeIsConsistentUsingLegacyCallbacks) {
  auto& context = GetEmbedderContext(EmbedderTestContextType::kSoftwareContext);

  fml::AutoResetWaitableEvent signal_native_latch;

  // Called by the Dart text fixture on the UI thread to signal that the C++
  // unittest should resume.
  context.AddNativeCallback(
      "SignalNativeTest",
      CREATE_NATIVE_ENTRY(([&signal_native_latch](Dart_NativeArguments) {
        signal_native_latch.Signal();
      })));

  // Called by test fixture on UI thread to pass data back to this test.
  NativeEntry notify_semantics_enabled_callback;
  context.AddNativeCallback(
      "NotifySemanticsEnabled",
      CREATE_NATIVE_ENTRY(
          ([&notify_semantics_enabled_callback](Dart_NativeArguments args) {
            ASSERT_NE(notify_semantics_enabled_callback, nullptr);
            notify_semantics_enabled_callback(args);
          })));

  NativeEntry notify_accessibility_features_callback;
  context.AddNativeCallback(
      "NotifyAccessibilityFeatures",
      CREATE_NATIVE_ENTRY((
          [&notify_accessibility_features_callback](Dart_NativeArguments args) {
            ASSERT_NE(notify_accessibility_features_callback, nullptr);
            notify_accessibility_features_callback(args);
          })));

  NativeEntry notify_semantics_action_callback;
  context.AddNativeCallback(
      "NotifySemanticsAction",
      CREATE_NATIVE_ENTRY(
          ([&notify_semantics_action_callback](Dart_NativeArguments args) {
            ASSERT_NE(notify_semantics_action_callback, nullptr);
            notify_semantics_action_callback(args);
          })));

  fml::AutoResetWaitableEvent semantics_node_latch;
  fml::AutoResetWaitableEvent semantics_action_latch;

  int node_batch_end_count = 0;
  int action_batch_end_count = 0;

  int node_count = 0;
  context.SetSemanticsNodeCallback([&](const FlutterSemanticsNode* node) {
    if (node->id == kFlutterSemanticsNodeIdBatchEnd) {
      ++node_batch_end_count;
      semantics_node_latch.Signal();
    } else {
      // Batches should be completed after all nodes are received.
      ASSERT_EQ(0, node_batch_end_count);
      ASSERT_EQ(0, action_batch_end_count);

      ++node_count;
      ASSERT_EQ(1.0, node->transform.scaleX);
      ASSERT_EQ(2.0, node->transform.skewX);
      ASSERT_EQ(3.0, node->transform.transX);
      ASSERT_EQ(4.0, node->transform.skewY);
      ASSERT_EQ(5.0, node->transform.scaleY);
      ASSERT_EQ(6.0, node->transform.transY);
      ASSERT_EQ(7.0, node->transform.pers0);
      ASSERT_EQ(8.0, node->transform.pers1);
      ASSERT_EQ(9.0, node->transform.pers2);
      ASSERT_EQ(std::strncmp(kTooltip, node->tooltip, sizeof(kTooltip) - 1), 0);

      if (node->id == 128) {
        ASSERT_EQ(0x3f3, node->platform_view_id);
      } else {
        ASSERT_EQ(0, node->platform_view_id);
      }
    }
  });

  int action_count = 0;
  context.SetSemanticsCustomActionCallback(
      [&](const FlutterSemanticsCustomAction* action) {
        if (action->id == kFlutterSemanticsCustomActionIdBatchEnd) {
          ++action_batch_end_count;
          semantics_action_latch.Signal();
        } else {
          // Batches should be completed after all actions are received.
          ASSERT_EQ(0, node_batch_end_count);
          ASSERT_EQ(0, action_batch_end_count);

          ++action_count;
        }
      });

  EmbedderConfigBuilder builder(context);
  builder.SetSoftwareRendererConfig();
  builder.SetDartEntrypoint("a11y_main");

  auto engine = builder.LaunchEngine();
  ASSERT_TRUE(engine.is_valid());

  // 1: Wait for initial notifySemanticsEnabled(false).
  fml::AutoResetWaitableEvent notify_semantics_enabled_latch;
  notify_semantics_enabled_callback = [&](Dart_NativeArguments args) {
    Dart_Handle exception = nullptr;
    bool enabled =
        ::tonic::DartConverter<bool>::FromArguments(args, 0, exception);
    ASSERT_FALSE(enabled);
    notify_semantics_enabled_latch.Signal();
  };
  notify_semantics_enabled_latch.Wait();

  // Prepare notifyAccessibilityFeatures callback.
  fml::AutoResetWaitableEvent notify_features_latch;
  notify_accessibility_features_callback = [&](Dart_NativeArguments args) {
    Dart_Handle exception = nullptr;
    bool enabled =
        ::tonic::DartConverter<bool>::FromArguments(args, 0, exception);
    ASSERT_FALSE(enabled);
    notify_features_latch.Signal();
  };

  // 2: Enable semantics. Wait for notifySemanticsEnabled(true).
  fml::AutoResetWaitableEvent notify_semantics_enabled_latch_2;
  notify_semantics_enabled_callback = [&](Dart_NativeArguments args) {
    Dart_Handle exception = nullptr;
    bool enabled =
        ::tonic::DartConverter<bool>::FromArguments(args, 0, exception);
    ASSERT_TRUE(enabled);
    notify_semantics_enabled_latch_2.Signal();
  };
  auto result = FlutterEngineUpdateSemanticsEnabled(engine.get(), true);
  ASSERT_EQ(result, FlutterEngineResult::kSuccess);
  notify_semantics_enabled_latch_2.Wait();

  // 3: Wait for notifyAccessibilityFeatures (reduce_motion == false)
  notify_features_latch.Wait();

  // 4: Wait for notifyAccessibilityFeatures (reduce_motion == true)
  fml::AutoResetWaitableEvent notify_features_latch_2;
  notify_accessibility_features_callback = [&](Dart_NativeArguments args) {
    Dart_Handle exception = nullptr;
    bool enabled =
        ::tonic::DartConverter<bool>::FromArguments(args, 0, exception);
    ASSERT_TRUE(enabled);
    notify_features_latch_2.Signal();
  };
  result = FlutterEngineUpdateAccessibilityFeatures(
      engine.get(), kFlutterAccessibilityFeatureReduceMotion);
  ASSERT_EQ(result, FlutterEngineResult::kSuccess);
  notify_features_latch_2.Wait();

  // 5: Wait for UpdateSemantics callback on platform (current) thread.
  signal_native_latch.Wait();
  fml::MessageLoop::GetCurrent().RunExpiredTasksNow();
  semantics_node_latch.Wait();
  semantics_action_latch.Wait();
  ASSERT_EQ(4, node_count);
  ASSERT_EQ(1, node_batch_end_count);
  ASSERT_EQ(1, action_count);
  ASSERT_EQ(1, action_batch_end_count);

  // 6: Dispatch a tap to semantics node 42. Wait for NotifySemanticsAction.
  fml::AutoResetWaitableEvent notify_semantics_action_latch;
  notify_semantics_action_callback = [&](Dart_NativeArguments args) {
    Dart_Handle exception = nullptr;
    int64_t node_id =
        ::tonic::DartConverter<int64_t>::FromArguments(args, 0, exception);
    ASSERT_EQ(42, node_id);

    int64_t action_id =
        ::tonic::DartConverter<int64_t>::FromArguments(args, 1, exception);
    ASSERT_EQ(static_cast<int32_t>(flutter::SemanticsAction::kTap), action_id);

    std::vector<int64_t> semantic_args =
        ::tonic::DartConverter<std::vector<int64_t>>::FromArguments(args, 2,
                                                                    exception);
    ASSERT_THAT(semantic_args, ElementsAre(2, 1));
    notify_semantics_action_latch.Signal();
  };
  std::vector<uint8_t> bytes({2, 1});
  result = FlutterEngineDispatchSemanticsAction(
      engine.get(), 42, kFlutterSemanticsActionTap, &bytes[0], bytes.size());
  ASSERT_EQ(result, FlutterEngineResult::kSuccess);
  notify_semantics_action_latch.Wait();

  // 7: Disable semantics. Wait for NotifySemanticsEnabled(false).
  fml::AutoResetWaitableEvent notify_semantics_enabled_latch_3;
  notify_semantics_enabled_callback = [&](Dart_NativeArguments args) {
    Dart_Handle exception = nullptr;
    bool enabled =
        ::tonic::DartConverter<bool>::FromArguments(args, 0, exception);
    ASSERT_FALSE(enabled);
    notify_semantics_enabled_latch_3.Signal();
  };
  result = FlutterEngineUpdateSemanticsEnabled(engine.get(), false);
  ASSERT_EQ(result, FlutterEngineResult::kSuccess);
  notify_semantics_enabled_latch_3.Wait();
}

}  // namespace testing
}  // namespace flutter

// NOLINTEND(clang-analyzer-core.StackAddressEscape)
